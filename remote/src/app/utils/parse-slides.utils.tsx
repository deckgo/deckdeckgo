import {h, JSX} from '@stencil/core';

import {v4 as uuid} from 'uuid';

import {DeckdeckgoDeckDefinition, DeckdeckgoSlideDefinition, DeckdeckgoAttributeDefinition} from '@deckdeckgo/types';

import {cleanContent} from '@deckdeckgo/deck-utils';

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
        promises.push(this.parseSlide(slide, index));
      });

      if (!promises || promises.length <= 0) {
        resolve(undefined);
        return;
      }

      const slides: JSX.IntrinsicElements[] = await Promise.all(promises);

      resolve(slides);
    });
  }

  static parseSlide(slide: DeckdeckgoSlideDefinition, index: number): Promise<JSX.IntrinsicElements> {
    return this.isSupportedTemplate(slide) ? this.parseSupportedSlide(slide) : this.parseDummySlide(index);
  }

  private static parseSupportedSlide(slide: DeckdeckgoSlideDefinition): Promise<JSX.IntrinsicElements> {
    return new Promise<JSX.IntrinsicElements>(async (resolve) => {
      const SlideElement: string = slide.template;

      let attributes: any = await ParseAttributesUtils.parseAttributes(slide.attributes);
      attributes = await this.setCustomAttributes(slide, attributes);

      // Create a div to parse back to JSX its children
      const div = document.createElement('div');
      div.innerHTML = await this.getCleanContent(slide);

      const content = await ParseElementsUtils.parseElements(div, true);

      const result: JSX.IntrinsicElements = (
        <SlideElement key={uuid()} {...attributes}>
          {content}
        </SlideElement>
      );

      resolve(result);
    });
  }

  private static getCleanContent(slide: DeckdeckgoSlideDefinition): Promise<string | undefined> {
    return new Promise<string | undefined>(async (resolve) => {
      if (!slide || !slide.content || slide.content === undefined || slide.content === '') {
        resolve(undefined);
        return;
      }

      let result: string = await cleanContent(slide.content);

      const customBackground: boolean = await this.hasCustomBackground(slide);

      if (!customBackground) {
        result = result.replace(/<div slot="background">(.*?)<\/div>/g, '');
      }

      result = result.replace(/<a slot="actions"(.*?)<\/a>/g, '');
      result = result.replace(/<!--.*?-->/g, '');

      resolve(result);
    });
  }

  private static hasCustomBackground(slide: DeckdeckgoSlideDefinition): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      let customBackground: boolean = false;
      if (slide.attributes && slide.attributes.length > 0) {
        let attr: DeckdeckgoAttributeDefinition = slide.attributes.find((attr: DeckdeckgoAttributeDefinition) => {
          return attr.name && (attr.name.toLowerCase() === 'custom-background' || attr.name === 'customBackground');
        });

        customBackground = attr !== null;
      }

      resolve(customBackground);
    });
  }

  private static isSupportedTemplate(slide: DeckdeckgoSlideDefinition): boolean {
    return (
      slide.template &&
      [
        'deckgo-slide-title',
        'deckgo-slide-author',
        'deckgo-slide-code',
        'deckgo-slide-chart',
        'deckgo-slide-split',
        'deckgo-slide-qrcode',
        'deckgo-slide-content',
        'deckgo-slide-gif',
        'deckgo-slide-countdown',
        'deckgo-slide-youtube',
        'deckgo-slide-big-img',
        'deckgo-slide-video',
        'deckgo-slide-poll',
        'deckgo-slide-aspect-ratio',
        'deckgo-slide-playground'
      ].indexOf(slide.template) > -1
    );
  }

  private static parseDummySlide(index: number): Promise<JSX.IntrinsicElements> {
    return new Promise<JSX.IntrinsicElements>(async (resolve) => {
      const SlideElement: string = 'deckgo-slide-title';

      const result: JSX.IntrinsicElements = (
        <SlideElement key={uuid()}>
          <h1 slot="title">Slide {index}</h1>
        </SlideElement>
      );

      resolve(result);
    });
  }

  private static setCustomAttributes(slide: DeckdeckgoSlideDefinition, attributes: any): Promise<any> {
    return new Promise<any>((resolve) => {
      if (slide.template && 'deckgo-slide-youtube' === slide.template) {
        attributes['allowFullscreen'] = false;
      }

      if (slide.template && 'deckgo-slide-chart' === slide.template) {
        attributes['marginTop'] = 8;
        attributes['marginBottom'] = 8;
        attributes['marginLeft'] = 8;
        attributes['marginRight'] = 8;
      }

      resolve(attributes);
    });
  }
}
