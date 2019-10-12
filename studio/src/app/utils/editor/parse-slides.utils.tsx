import {h, JSX} from '@stencil/core';

import {ParseStyleUtils} from './parse-style.utils';
import {ParseElementsUtils} from './parse-elements.utils';

import {QRCodeUtils} from './qrcode.utils';

import {Slide, SlideTemplate} from '../../models/data/slide';
import {Deck} from '../../models/data/deck';

export class ParseSlidesUtils {

    static parseSlide(deck: Deck, slide: Slide, contentEditable: boolean, ignoreSlideId: boolean = false): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>(async (resolve) => {
            if (!document || !slide || !slide.data || !slide.data.template) {
                resolve(null);
                return;
            }

            if (SlideTemplate[slide.data.template.toUpperCase()]) {
                resolve(await this.parseSlideElement(deck, slide, `deckgo-slide-${SlideTemplate[slide.data.template.toUpperCase()].toLowerCase()}`, contentEditable, ignoreSlideId));
            } else {
                resolve(null);
            }
        });
    }

    private static parseSlideElement(deck: Deck, slide: Slide, slideTag: string, contentEditable: boolean, ignoreSlideId: boolean): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>(async (resolve) => {
            if (!document) {
                resolve();
                return;
            }

            // Create a div to parse back to JSX its children
            const div = document.createElement('div');
            div.innerHTML = slide.data.content;

            const content = await ParseElementsUtils.parseElements(div, true, contentEditable);

            const attributes = {
                style: slide.data.attributes ? await ParseStyleUtils.convertStyle(slide.data.attributes.style) : undefined,
                src: slide.data.attributes && slide.data.attributes.src ? slide.data.attributes.src : undefined,
                'custom-background': slide.data.attributes && slide.data.attributes.customBackground ? slide.data.attributes.customBackground : undefined,
                'img-src': slide.data.attributes && slide.data.attributes.imgSrc ? slide.data.attributes.imgSrc : undefined,
                'img-alt': slide.data.attributes && slide.data.attributes.imgAlt ? slide.data.attributes.imgAlt : undefined
            };

            if (slide.data.template === SlideTemplate.QRCODE) {
                attributes['content'] = slide.data.attributes && slide.data.attributes.content ? slide.data.attributes.content : QRCodeUtils.getPresentationUrl(deck);
                attributes['custom-qrcode'] = slide.data.attributes && slide.data.attributes.content ? 'true' : undefined;
            }

            if (slide.data.template === SlideTemplate.CHART) {
                attributes['type'] = slide.data.attributes && slide.data.attributes.type ? slide.data.attributes.type : undefined;
                attributes['inner-radius'] = slide.data.attributes && slide.data.attributes.innerRadius ? slide.data.attributes.innerRadius : undefined;
                attributes['animation'] = slide.data.attributes && slide.data.attributes.animation ? slide.data.attributes.animation : undefined;
                attributes['date-pattern'] = slide.data.attributes && slide.data.attributes.datePattern ? slide.data.attributes.datePattern : undefined;
                attributes['y-axis-domain'] = slide.data.attributes && slide.data.attributes.yAxisDomain ? slide.data.attributes.yAxisDomain : undefined;
                attributes['smooth'] = slide.data.attributes && slide.data.attributes.smooth === false ? 'false' : undefined;
                attributes['area'] = slide.data.attributes && slide.data.attributes.area === false ? 'false' : undefined;
                attributes['ticks'] = slide.data.attributes && slide.data.attributes.ticks ? slide.data.attributes.ticks : undefined;
                attributes['grid'] = slide.data.attributes && slide.data.attributes.grid ? 'true' : undefined;
            }

            const SlideElement: string = slideTag;

            const result: JSX.IntrinsicElements = <SlideElement key={slide.id} slide_id={ignoreSlideId ? undefined : slide.id} {...attributes}>
                {content}
            </SlideElement>;

            resolve(result);
        });
    }
}
