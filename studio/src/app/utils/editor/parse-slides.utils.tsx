import {h} from '@stencil/core';

import {ParseStyleUtils} from './parse-style.utils';
import {ParseElementsUtils} from './parse-elements.utils';
import {Slide, SlideTemplate} from '../../models/data/slide';

export class ParseSlidesUtils {

    static parseSlide(slide: Slide): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!document || !slide || !slide.data || !slide.data.template) {
                resolve(null);
                return;
            }

            if (SlideTemplate[slide.data.template.toUpperCase()] === SlideTemplate.TITLE) {
                resolve(await this.parseSlideElement(slide, 'deckgo-slide-title'));
            } else if (SlideTemplate[slide.data.template.toUpperCase()] === SlideTemplate.CONTENT) {
                resolve(await this.parseSlideElement(slide, 'deckgo-slide-content'));
            } else if (SlideTemplate[slide.data.template.toUpperCase()] === SlideTemplate.SPLIT) {
                resolve(await this.parseSlideElement(slide, 'deckgo-slide-split'));
            } else if (SlideTemplate[slide.data.template.toUpperCase()] === SlideTemplate.GIF) {
                resolve(await this.parseSlideElement(slide, 'deckgo-slide-gif'));
            } else if (SlideTemplate[slide.data.template.toUpperCase()] === SlideTemplate.AUTHOR) {
                resolve(await this.parseSlideElement(slide, 'deckgo-slide-author'));
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
            div.innerHTML = slide.data.content;

            const content = await ParseElementsUtils.parseElements(div, true);

            const style = slide.data.attributes ? await ParseStyleUtils.convertStyle(slide.data.attributes.style) : undefined;

            const src = slide.data.attributes && slide.data.attributes.src ? slide.data.attributes.src : undefined;

            const customBackground = slide.data.attributes && slide.data.attributes.customBackground ? slide.data.attributes.customBackground : undefined;

            const imgSrc = slide.data.attributes && slide.data.attributes.imgSrc ? slide.data.attributes.imgSrc : undefined;
            const imgAlt = slide.data.attributes && slide.data.attributes.imgAlt ? slide.data.attributes.imgAlt : undefined;

            const SlideElement: string = slideTag;

            // @ts-ignore
            const result: any = <SlideElement key={slide.id} slide_id={slide.id} style={style} src={src} custom-background={customBackground} img-src={imgSrc} img-alt={imgAlt}>
                {content}
            </SlideElement>;

            resolve(result);
        });
    }
}
