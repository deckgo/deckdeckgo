import {Slide} from '../../models/slide';
import {SlideTemplate} from '../../models/slide-template';

export class ParseSlidesUtils {

    static parseSlide(slide: Slide): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!document || !slide || !slide.template) {
                resolve(null);
                return;
            }

            // TODO: other types

            if (SlideTemplate[slide.template.toUpperCase()] === SlideTemplate.TITLE) {
                resolve(await this.parseSlideElement(slide, 'deckgo-slide-title'));
            } else if (SlideTemplate[slide.template.toUpperCase()] === SlideTemplate.CONTENT) {
                resolve(await this.parseSlideElement(slide, 'deckgo-slide-content'));
            } else if (SlideTemplate[slide.template.toUpperCase()] === SlideTemplate.SPLIT) {
                resolve(await this.parseSlideElement(slide, 'deckgo-slide-split'));
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

            const content = await this.parseElements(div);

            const style = slide.attributes ? await this.convertStyle(slide.attributes.style) : undefined;

            const SlideElement: string = slideTag;

            // @ts-ignore
            const result: any = <SlideElement slide_id={slide.id} style={style}>
                {content}
            </SlideElement>;

            resolve(result);
        });
    }

    private static parseElements(element: HTMLElement): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!parent) {
                resolve(undefined);
                return;
            }

            if (element.nodeType === 3) {
                resolve(await this.parseElement(element.parentElement, element.textContent));
                return;
            }

            if (element.hasChildNodes()) {
                const results = [];

                const elements: HTMLElement[] = Array.prototype.slice.call(element.childNodes);

                for (const elem of elements) {
                    results.push(await this.parseElements(elem));
                }

                resolve(results);
            } else {
                resolve(await this.parseElement(element, element.textContent));
            }
        });
    }

    private static parseElement(element: HTMLElement, content: string): Promise<any> {
        return new Promise<any>(async (resolve) => {
            const Elem: string = element.nodeName;

            const attributes: any = this.getAttributes(element);
            if (attributes.style) {
                attributes.style = await this.convertStyle(attributes.style);
            }

            resolve(<Elem {...attributes}>{content}</Elem>);
        });
    }

    private static convertStyle(originalStyle: string): Promise<any> {
        return new Promise<any>((resolve) => {
            if (!originalStyle || originalStyle.length <= 0) {
                resolve(undefined);
                return;
            }

            const result: any = {};

            const styles: string[] = originalStyle.split(';');

            if (styles && styles.length > 0) {
                styles.forEach((style: string) => {
                    if (style && style.length > 0) {
                        const split: string[] = style.split(':');
                        if (split && split.length > 1) {
                            result[split[0].trim()] = split[1].trim();
                        } else if (split && split.length > 0) {
                            result[split[0].trim()] = undefined;
                        }
                    }
                });
            }

            resolve(result);
        });
    }

    private static getAttributes(el) {
        return Array.from(el.attributes)
            .map((a: Attr) => [a.name, a.value])
            .reduce((acc, attr) => {
                acc[attr[0]] = attr[1];
                return acc
            }, {});
    }
}
