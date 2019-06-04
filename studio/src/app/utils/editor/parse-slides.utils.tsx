import {h} from '@stencil/core';

import {Slide, SlideTemplate} from '../../models/slide';
import {ParseStyleUtils} from './parse-style.utils';
import {ParseElementsUtils} from './parse-elements.utils';

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

            const content = await ParseElementsUtils.parseElements(div, true);

            const style = slide.attributes ? await ParseStyleUtils.convertStyle(slide.attributes.style) : undefined;

            const src = slide.attributes && slide.attributes.src ? slide.attributes.src : undefined;

            const customBackground = slide.attributes && slide.attributes.customBackground ? slide.attributes.customBackground : undefined;

            const SlideElement: string = slideTag;

            // @ts-ignore
            const result: any = <SlideElement slide_id={slide.id} style={style} src={src} custom-background={customBackground}>
                {content}
            </SlideElement>;

            resolve(result);
        });
    }
}
