import * as functions from 'firebase-functions';

import {Deck} from '../../model/data/deck';
import {ApiDeck} from '../../model/api/api.deck';
import {ApiSlide} from '../../model/api/api.slide';
import {Slide, SlideAttributes, SlideTemplate} from '../../model/data/slide';

import {findSlide} from '../../utils/data/slide-utils';
import {getGoogleFontScript} from './google-fonts-utils';

export function convertDeck(deck: Deck): Promise<ApiDeck> {
  return new Promise<ApiDeck>(async (resolve, reject) => {
    try {
      const apiSlides: ApiSlide[] = await convertSlides(deck);

      const apiDeck: ApiDeck = {
        name: deck.data.name ? deck.data.name.trim() : deck.data.name,
        description:
          deck.data.meta && deck.data.meta.description !== undefined && deck.data.meta.description !== ''
            ? (deck.data.meta.description as string)
            : deck.data.name,
        owner_id: deck.data.owner_id,
        attributes: deck.data.attributes,
        background: deck.data.background,
        header: deck.data.header,
        footer: deck.data.footer,
        slides: apiSlides,
      };

      const googleFontScript: string | undefined = await getGoogleFontScript(deck);
      if (googleFontScript !== undefined) {
        apiDeck.head_extra = googleFontScript;
      }

      resolve(apiDeck);
    } catch (err) {
      reject(err);
    }
  });
}

function convertSlides(deck: Deck): Promise<ApiSlide[]> {
  return new Promise<ApiSlide[]>(async (resolve, reject) => {
    if (!deck.data.slides || deck.data.slides.length <= 0) {
      resolve([]);
      return;
    }

    try {
      const promises: Promise<ApiSlide>[] = [];

      for (let i: number = 0; i < deck.data.slides.length; i++) {
        const slideId: string = deck.data.slides[i];

        promises.push(convertSlide(deck, slideId));
      }

      if (!promises || promises.length <= 0) {
        resolve([]);
        return;
      }

      const slides: ApiSlide[] = await Promise.all(promises);

      resolve(slides);
    } catch (err) {
      reject(err);
    }
  });
}

function convertSlide(deck: Deck, slideId: string): Promise<ApiSlide> {
  return new Promise<ApiSlide>(async (resolve, reject) => {
    const slide: Slide = await findSlide(deck.id, slideId);

    if (!slide || !slide.data) {
      reject('Missing slide for publishing');
      return;
    }

    const attributes: SlideAttributes = await convertAttributesToString(slide.data.attributes);

    const apiSlide: ApiSlide = {
      template: slide.data.template,
      content: slide.data.content,
      attributes: attributes,
    };

    const cleanApiSlide: ApiSlide = await convertSlideQRCode(apiSlide);
    cleanApiSlide.content = await cleanNotes(apiSlide.content);

    resolve(cleanApiSlide);
  });
}

function convertAttributesToString(attributes: SlideAttributes | undefined): Promise<SlideAttributes> {
  return new Promise<SlideAttributes>((resolve) => {
    if (!attributes) {
      resolve(undefined);
      return;
    }

    // We loose the type but doing so we ensure that all attributes are converted to string in order to parse them to HTML in the API
    const result: SlideAttributes = {};
    Object.keys(attributes).forEach((key: string) => {
      // @ts-ignore
      result[key] = `${attributes[key]}`;
    });

    if (!result) {
      resolve(undefined);
      return;
    }

    resolve(result);
  });
}

function cleanNotes(content: string | undefined): Promise<string> {
  return new Promise<string>((resolve) => {
    if (!content || content === undefined || content === '') {
      resolve(content);
      return;
    }

    const result: string = content.replace(/<div slot="notes".*?>(.|[\s\S])*?<\/div>/gi, '');

    resolve(result);
  });
}

function convertSlideQRCode(apiSlide: ApiSlide): Promise<ApiSlide> {
  return new Promise<ApiSlide>(async (resolve) => {
    if (!apiSlide) {
      resolve(apiSlide);
      return;
    }

    if (apiSlide.template !== SlideTemplate.QRCODE) {
      resolve(apiSlide);
      return;
    }

    const presentationUrl: string = functions.config().deckdeckgo.presentation.url;

    // If no attributes at all, we create an attribute "content" of the QR code with it's upcoming published url
    if (!apiSlide.attributes) {
      apiSlide.attributes = {
        content: `${presentationUrl}{{DECKDECKGO_BASE_HREF}}`,
      };
    }

    // If not custom content, we replace the attribute "content" of the QR code with it's upcoming published url
    if (!apiSlide.attributes.hasOwnProperty('customQRCode') || !apiSlide.attributes.customQRCode) {
      apiSlide.attributes.content = `${presentationUrl}{{DECKDECKGO_BASE_HREF}}`;
    }

    // In any case, we don't need customQRCode attribute in our presentations, this is an attribute used by the editor
    if (apiSlide.attributes.hasOwnProperty('customQRCode')) {
      delete apiSlide.attributes['customQRCode'];
    }

    resolve(apiSlide);
  });
}
