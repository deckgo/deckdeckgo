import {Slide, SlideTemplate} from '../../models/slide';
import {ParseStyleUtils} from './parse-style.utils';
import {SlotType} from './create-slides.utils';

export class ParseSlidesUtils {

    static parseSlide(slide: Slide): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!document || !slide || !slide.template) {
                resolve(null);
                return;
            }

            if (SlideTemplate[slide.template.toUpperCase()] === SlideTemplate.TITLE) {
                resolve(await this.parseSlideElement(slide, 'deckgo-slide-title'));
            } else if (SlideTemplate[slide.template.toUpperCase()] === SlideTemplate.CONTENT) {
                resolve(await this.parseSlideElement(slide, 'deckgo-slide-content'));
            } else if (SlideTemplate[slide.template.toUpperCase()] === SlideTemplate.SPLIT) {
                resolve(await this.parseSlideElement(slide, 'deckgo-slide-split'));
            } else if (SlideTemplate[slide.template.toUpperCase()] === SlideTemplate.GIF) {
                resolve(await this.parseSlideElement(slide, 'deckgo-slide-gif'));
            } else {
                resolve(null);
            }
        });
    }

    private static parseSlideElement(slide: Slide, slideTag: string): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!document) {
                resolve();
                return;
            }

            // Create a div to parse back to JSX its children
            const div = document.createElement('div');
            div.innerHTML = slide.content;

            const content = await this.parseElements(div, true);

            const style = slide.attributes ? await ParseStyleUtils.convertStyle(slide.attributes.style) : undefined;

            const src = slide.attributes && slide.attributes.src ? slide.attributes.src : undefined;

            const SlideElement: string = slideTag;

            // @ts-ignore
            const result: any = <SlideElement slide_id={slide.id} style={style} src={src}>
                {content}
            </SlideElement>;

            resolve(result);
        });
    }

    private static parseElements(element: HTMLElement, root: boolean): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!element) {
                resolve(undefined);
                return;
            }

            if (element.nodeType === 3) {
                resolve(element.textContent + '\n');
                return;
            }

            if (element.hasChildNodes()) {
                const results = [];

                const elements: HTMLElement[] = Array.prototype.slice.call(element.childNodes);

                for (const elem of elements) {
                    const result = await this.parseElements(elem, false);
                    results.push(result);
                }

                resolve(root ? results : await this.parseElement(element, results));
            } else {
                resolve(await this.parseElement(element, element.textContent));
            }
        });
    }

    private static parseElement(element: HTMLElement, content: any): Promise<any> {
        return new Promise<any>(async (resolve) => {
            const Elem: string = element.nodeName;

            const attributes: any = this.getAttributes(element);
            if (attributes.style) {
                attributes.style = await ParseStyleUtils.convertStyle(attributes.style);
            }

            if (attributes.slot && (!element.nodeName || (element.nodeName.toLowerCase() !== 'code' && element.nodeName.toLowerCase() !== SlotType.CODE))) {
                attributes['contenteditable'] = true;
            }

            if (element.nodeName&& element.nodeName.toLowerCase() === 'deckgo-lazy-img') {
                attributes['contenteditable'] = 'false';
            }

            resolve(<Elem {...attributes}>{content}</Elem>);
        });
    }

    private static getAttributes(el): any {
        if (!el || !el.attributes) {
            return {};
        }

        return Array.from(el.attributes)
            .map((a: Attr) => [a.name, a.value])
            .reduce((acc, attr) => {
                acc[attr[0]] = attr[1];
                return acc
            }, {});
    }
}
