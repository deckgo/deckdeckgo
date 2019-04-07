import {Slide} from '../../models/slide';
import {SlideTemplate} from '../../models/slide-template';
import {SlideAttributes} from '../../models/slide-attributes';

export class ParseSlidesUtils {

    static parseSlide(slide: Slide): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!document || !slide || !slide.template) {
                resolve(null);
                return;
            }

            if (SlideTemplate[slide.template.toUpperCase()] === SlideTemplate.TITLE) {
                resolve(await this.parseSlideTitle(slide));
            } else {
                resolve(null);
            }
        });
    }

    private static parseSlideTitle(slide: Slide): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!document) {
                resolve();
                return;
            }

            // const Content: string = 'h3';
            // const props = {style: {background: 'red'}, slot: 'content'};
            //
            // const div = document.createElement('div');
            // div.innerHTML = slide.content;
            //
            // console.log(div, (div.firstChild as HTMLElement).attributes);
            //
            // const Title: string = (div.firstChild as HTMLElement).nodeName;

            const div = document.createElement('div');
            div.innerHTML = slide.content;

            const content = await this.parseElements(div);

            console.log('content', content);

            const style = await this.convertStyle(slide.attributes);

            // @ts-ignore
            const result: any = <deckgo-slide-title slide_id={slide.id} style={style}>
                {content}
            </deckgo-slide-title>;

            resolve(result);
        });
    }

    private static parseElements(element: HTMLElement): Promise<any> {
        return new Promise<any>(async (resolve) => {

            console.log('parse', element);

            if (!parent) {
                resolve(undefined);
                return;
            }

            if (element.nodeType === 3) {
                const Elem: string = element.parentElement.nodeName;
                resolve(<Elem {...element.parentElement.attributes}>{element.textContent}</Elem>);
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
                const Elem: string = element.nodeName;
                resolve(<Elem {...element.attributes}></Elem>);
            }
        });
    }

    private static convertStyle(attributes: SlideAttributes): Promise<any> {
        return new Promise<any>((resolve) => {
            if (!attributes || !attributes.style || attributes.style.length <= 0) {
                resolve(undefined);
                return;
            }

            const result: any = {};

            const styles: string[] = attributes.style.split(';');
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
}
