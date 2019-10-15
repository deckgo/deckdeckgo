import {h, JSX} from '@stencil/core';

import uuid from 'uuid/v4';

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

            const style = slide.data.attributes ? await ParseStyleUtils.convertStyle(slide.data.attributes.style) : undefined;

            const src = slide.data.attributes && slide.data.attributes.src ? slide.data.attributes.src : undefined;

            const customBackground = slide.data.attributes && slide.data.attributes.customBackground ? slide.data.attributes.customBackground : undefined;

            let contentAttr = undefined;
            let customQRCode = undefined;

            if (slide.data.template === SlideTemplate.QRCODE) {
                contentAttr = slide.data.attributes && slide.data.attributes.content ? slide.data.attributes.content : QRCodeUtils.getPresentationUrl(deck);
                customQRCode = slide.data.attributes && slide.data.attributes.content ? 'true' : undefined;
            }

            const imgSrc = slide.data.attributes && slide.data.attributes.imgSrc ? slide.data.attributes.imgSrc : undefined;
            const imgAlt = slide.data.attributes && slide.data.attributes.imgAlt ? slide.data.attributes.imgAlt : undefined;

            const SlideElement: string = slideTag;

            const result: JSX.IntrinsicElements = <SlideElement key={uuid()} slide_id={ignoreSlideId ? undefined : slide.id} style={style}
                                                                src={src} custom-background={customBackground} img-src={imgSrc} img-alt={imgAlt} content={contentAttr} custom-qrcode={customQRCode}>
                {content}
            </SlideElement>;

            resolve(result);
        });
    }
}
