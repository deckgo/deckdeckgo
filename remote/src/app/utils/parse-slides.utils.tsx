import {h, JSX} from '@stencil/core';

import uuid from 'uuid/v4';

import {DeckdeckgoDeckDefinition, DeckdeckgoSlideDefinition} from '@deckdeckgo/types';

import {ParseElementsUtils} from './parse-elements.utils';
import {ParseAttributesUtils} from './parse-attributes.utils';

export class ParseSlidesUtils {

    static parseSlides(deck: DeckdeckgoDeckDefinition): Promise<JSX.IntrinsicElements[] | undefined> {
        return new Promise<JSX.IntrinsicElements[] | undefined>(async (resolve) => {
            if (!deck || !deck.slides || deck.slides.length <= 0) {
                resolve(undefined);
                return;
            }

            const promises: Promise<JSX.IntrinsicElements | undefined>[] = [];
            deck.slides.forEach((slide: DeckdeckgoSlideDefinition, index: number) => {
                promises.push(this.isSupportedTemplate(slide) ? this.parseSlide(slide) : this.parseDummySlide(index));
            });

            if (!promises || promises.length <= 0) {
                resolve(undefined);
                return;
            }

            const slides: JSX.IntrinsicElements[] = await Promise.all(promises);

            resolve(slides);
        });

    }

    private static parseSlide(slide: DeckdeckgoSlideDefinition): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>(async (resolve) => {
            const SlideElement: string = slide.template;

            const attributes: any = await ParseAttributesUtils.parseAttributes(slide.attributes);

            // Create a div to parse back to JSX its children
            const div = document.createElement('div');
            div.innerHTML = slide.content;

            const content = await ParseElementsUtils.parseElements(div, true);

            const result: JSX.IntrinsicElements = <SlideElement key={uuid()} {...attributes}>
                {content}
            </SlideElement>;

            resolve(result);
        });
    }

    private static isSupportedTemplate(slide: DeckdeckgoSlideDefinition): boolean {
        return slide.template && ['deckgo-slide-title'].indexOf(slide.template) > -1;
    }

    private static parseDummySlide(index: number): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>(async (resolve) => {
            const SlideElement: string = 'deckgo-slide-title';

            const result: JSX.IntrinsicElements = <SlideElement key={uuid()}>
                <h1 slot="title">Slide {index}</h1>
            </SlideElement>;

            resolve(result);
        });
    }
}
