const resources = require('./deckdeckgo-markdown-resources');

const fs = require('fs');
const path = require('path');
const readline = require('readline');

const {Remarkable} = require('remarkable');
const {replaceEntities, escapeHtml, unescapeMd} = require('remarkable');

const util = require('util');
const readFile = util.promisify(fs.readFile);
const writeFile = util.promisify(fs.writeFile);

interface DeckDeckGoMarkdownPluginOptions {
    src: string;
}

class DeckDeckGoMarkdownPlugin {

    private currentSlideTag: string;

    private slotTitleFound: boolean = false;
    private slotContentFound: boolean = false;
    private slotFirstFound: boolean = false;
    private slotSecondFound: boolean = false;
    private slotNotesFound: boolean = false;

    private codeComponentFound: boolean = false;

    private md;

    private readonly options: DeckDeckGoMarkdownPluginOptions;

    constructor(options?: DeckDeckGoMarkdownPluginOptions) {
        this.options = options;
    }

    apply(compiler) {

        // Hook a watch on the source markdown file
        compiler.hooks.afterCompile.tap('after-compile', (compilation) => {
            compilation.fileDependencies.add(this.getSrc());
        });

        // Parse the markdown file into the index.html
        compiler.hooks.afterEmit.tap('DeckDeckGoMarkdownPlugin', async () => {
            const indexMd: boolean = fs.existsSync(this.getSrc());
            if (indexMd) {
                await this.processLineByLine();
            }
        });
    }

    /**
     * Path per default index.md if no src provided
     */
    private getSrc(): string {
        return this.options && this.options.src ? this.options.src : path.resolve(resources.Constants.SRC);
    }

    private renderMarkdown(line: string): string {
        if (!this.md) {
            this.md = new Remarkable({
                html: true,
                xhtmlOut: true
            });

            const imageRule = () => (tokens, idx, options, env) => {
                const src: string = ' data-src="' + escapeHtml(tokens[idx].src) + '"';
                const title: string = tokens[idx].title ? (' title="' + escapeHtml(replaceEntities(tokens[idx].title)) + '"') : '';
                const alt: string = ' alt="' + (tokens[idx].alt ? escapeHtml(replaceEntities(unescapeMd(tokens[idx].alt))) : '') + '"';
                const suffix: string = options.xhtmlOut ? ' /' : '';
                return '<img' + src + alt + title + suffix + '>';
            };

            const htmlRule = (textRule, htmlBlockRule) => (tokens, i, opt, env) => {
                if (tokens && tokens.length >= 1 && tokens[0].content && resources.Constants.REGEX.IONIC_DECKDECKGO_HTML_PARSER.test(tokens[0].content)) {
                    return htmlBlockRule(tokens, i, opt, env);
                } else {
                    return textRule(tokens, i, opt, env);
                }
            };

            this.md.renderer.rules.image = imageRule();

            this.md.renderer.rules.text = htmlRule(this.md.renderer.rules.text, this.md.renderer.rules.htmlblock);
        }

        return this.md.render(line);
    }

    private async processLineByLine() {
        const fileStream = fs.createReadStream(this.getSrc());

        const rl = readline.createInterface({
            input: fileStream,
            crlfDelay: Infinity
        });

        let deckgoSlidesHtml: string = '';

        rl.on('line', (line: string) => {
            deckgoSlidesHtml += this.processLine(line);
        });

        rl.on('close', async () => {
            if (deckgoSlidesHtml && deckgoSlidesHtml.length > 0) {
                deckgoSlidesHtml += this.closeSlide();

                await this.parseSlides(deckgoSlidesHtml);
            }
        });
    }

    private async parseSlides(deckgoSlidesHtml: string) {
        try {
            const content = await readFile(resources.Constants.OUTPUT, 'utf8');

            let m: RegExpExecArray = resources.Constants.REGEX.DECK.exec(content);
            if (m && m.length >= 5) {
                const deckgoDeckTagBegin: string = m[1];
                const deckgoDeckTagEnd: string = m[4];

                const updatedContent = content.replace(resources.Constants.REGEX.DECK, deckgoDeckTagBegin + deckgoSlidesHtml + deckgoDeckTagEnd);

                writeFile(resources.Constants.OUTPUT, updatedContent, 'utf8');
            }
        } catch (e) {
            console.error(e);
        }
    }

    private processLine(line: string): string {
        let result: string = '';

        if (line) {
            const slide = resources.Constants.REGEX.SLIDE.SLIDE.test(line);

            if (slide) {
                result = this.closeSlide();

                result += this.createSlide(line);
            } else {
                result = this.createSlideContent(line);
            }
        }

        return result;
    }

    private createSlideContent(line: string): string {
        if (!this.slotTitleFound && resources.Constants.REGEX.SLOT.TITLE.test(line)) {
            return this.createSlotTitle(line);
        } else {
            return this.createOtherSlots(line);
        }
    };

    private createOtherSlots(line: string) {
        let result: string = '';

        if (this.currentSlideTag) {
            if (!this.slotContentFound && resources.Constants.REGEX.SLIDE.SLIDE_WITH_SLOT_CONTENT.test(this.currentSlideTag)) {
                result = this.createSlotContent();
            } else if (!this.slotSecondFound && resources.Constants.REGEX.SLIDE.SLIDE_WITH_TWO_SLOTS.test(this.currentSlideTag)) {
                const currentSlideTagSpit: boolean = resources.Constants.REGEX.SLIDE.SLIDE_SPLIT.test(this.currentSlideTag);

                if (!this.slotFirstFound) {
                    result = this.createFirstSlot(currentSlideTagSpit);
                } else if (resources.Constants.REGEX.SLOT.TITLE.test(line)) {
                    result = this.createSecondEnd(currentSlideTagSpit);
                }
            }
        }

        result += this.createContent(line);

        return result;
    }

    private createSecondEnd(slideSpit: boolean): string {
        let result: string = '';

        // If we found a title, we close the start slot and create the end slot
        if (this.slotFirstFound) {
            result = resources.Constants.TAG.SLOT.END + '\n';
        }

        result += (slideSpit ? resources.Constants.TAG.SLOT.SPLIT_END : resources.Constants.TAG.SLOT.GIF_FOOTER) + '\n';

        this.slotSecondFound = true;

        return result;
    }

    private createFirstSlot(slideSpit: boolean): string {
        this.slotFirstFound = true;
        return (slideSpit ? resources.Constants.TAG.SLOT.SPLIT_START : resources.Constants.TAG.SLOT.GIF_HEADER) + '\n';
    }

    private createSlotContent(): string {
        this.slotContentFound = true;
        return resources.Constants.TAG.SLOT.CONTENT + '\n';
    }

    private createSlotTitle(line: string) {
        let title: string = this.renderMarkdown(line);

        const result = title.replace(resources.Constants.REGEX.SLOT.SLOT, resources.Constants.TAG.SLOT.TITLE);

        this.slotTitleFound = true;

        return result;
    }

    private createSlide(line: string): string {
        let m: RegExpExecArray = resources.Constants.REGEX.SLIDE.SLIDE.exec(line);
        if (m && m.length >= 3) {
            const slideWithAttributes: string = m[1];
            const slide: string = m[2];

            this.currentSlideTag = resources.Constants.TAG.SLIDE + slide;

            this.slotTitleFound = false;
            this.slotContentFound = false;

            return '<' + resources.Constants.TAG.SLIDE + slideWithAttributes + '>\n';
        } else {
            return this.createContent(line) + '\n';
        }
    }

    private closeSlide(): string {
        let result: string = '';

        if (this.currentSlideTag) {
            result = this.closeOtherSlots(true);
            result += '</' + this.currentSlideTag + '>\n';

            this.currentSlideTag = null;
            this.slotTitleFound = false;
        }

        return result;
    }

    private closeOtherSlots(resetFlags: boolean): string {
        if (this.slotContentFound || this.slotFirstFound || this.slotSecondFound || this.slotNotesFound) {
            if (resetFlags) {
                this.slotContentFound = false;
                this.slotFirstFound = false;
                this.slotSecondFound = false;
                this.slotNotesFound = false;
            }

            return resources.Constants.TAG.SLOT.END + '\n';
        }

        return '';
    }

    private createContent(line: string): string {
        if (this.isLineSlotNotes(line)) {
            return this.createNotes(line);
        } else if (this.isLineCode(line)) {
            return this.createCode(line);
        } else {
            return this.codeComponentFound ? this.renderLineWithoutFormatting(line) : this.renderMarkdown(line);
        }
    }

    private isLineSlotNotes(line: string) {
        return resources.Constants.REGEX.SLOT.NOTES.test(line);
    }

    private createNotes(line: string): string {
        let result: string = this.closeOtherSlots(false);

        this.slotNotesFound = true;

        let m: RegExpExecArray = resources.Constants.REGEX.SLOT.NOTES.exec(line);

        const showAttributes: boolean = m && m.length >= 3 && m[2] !== undefined ? true : false;

        result += showAttributes ? resources.Constants.TAG.SLOT.NOTES_SHOW + '\n' : resources.Constants.TAG.SLOT.NOTES + '\n';

        return result;
    }

    private isLineCode(line: string) {
        return resources.Constants.REGEX.COMPONENT.CODE.test(line);
    }

    private createCode(line: string): string {
        let result;

        if (this.codeComponentFound) {
            result = resources.Constants.TAG.SLOT.CODE_END + '\n';
            result += '</' + resources.Constants.TAG.COMPONENT.CODE + '>\n';
        } else {
            result = '<' + resources.Constants.TAG.COMPONENT.CODE;

            const m: RegExpExecArray = resources.Constants.REGEX.COMPONENT.CODE.exec(line);

            if (m && m.length >= 3) {
                const attributes: string = m[2];
                if (attributes && attributes !== '') {
                    result += ' ' + attributes;
                }
            }

            result += '>\n';
            result += resources.Constants.TAG.SLOT.CODE_BEGIN + '\n';
        }

        this.codeComponentFound = !this.codeComponentFound;

        return result;
    }

    private renderLineWithoutFormatting(line: string): string {
        return this.escapeUnsafe(line) + '\n';
    }

    private escapeUnsafe(unsafe: string): string {
        return unsafe
            .replace(/&/g, "&amp;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;")
            .replace(/"/g, "&quot;")
            .replace(/'/g, "&#039;");
    }
}

module.exports = DeckDeckGoMarkdownPlugin;
