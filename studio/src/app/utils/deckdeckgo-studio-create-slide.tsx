import {SlideTemplate} from '../models/slide-template';

export class DeckdeckgoStudioCreateSlide {

    static DEFAULT_TITLE: string = 'Click to add title';
    static DEFAULT_CONTENT: string = 'Click to add content';

    static createSlide(template: SlideTemplate): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!document) {
                resolve(null);
                return;
            }

            if (template === SlideTemplate.TITLE) {
                resolve(await this.createSlideTitle());
            } else if (template === SlideTemplate.CONTENT) {
                resolve(await this.createSlideContent());
            } else if (template === SlideTemplate.SPLIT) {
                resolve(await this.createSlideSplit());
            } else {
                resolve(null);
            }
        });
    }

    private static createSlideTitle(): Promise<any> {
        return new Promise<any>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="title" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_TITLE}
            </h1>;

            const content = <p slot="content" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CONTENT}
            </p>;

            const slide: any = <deckgo-slide-title>
                {title}
                {content}
            </deckgo-slide-title>;

            resolve(slide);
        });
    }

    private static createSlideContent(): Promise<any> {
        return new Promise<any>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="title" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_TITLE}
            </h1>;

            const content = <p slot="content" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CONTENT}
            </p>;

            const slide: any = <deckgo-slide-content>
                {title}
                {content}
            </deckgo-slide-content>;

            resolve(slide);
        });
    }

    private static createSlideSplit(): Promise<any> {
        return new Promise<any>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="title" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_TITLE}
            </h1>;

            const start = <p slot="start" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CONTENT}
            </p>;

            const end = <p slot="end" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CONTENT}
            </p>;

            const style = {
                '--slide-split-title-display': 'visible'
            };

            const slide: any = <deckgo-slide-split style={style}>
                {title}
                {start}
                {end}
            </deckgo-slide-split>;

            resolve(slide);
        });
    }

}
