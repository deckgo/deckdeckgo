import * as functions from 'firebase-functions';

import {Deck} from '../../model/data/deck';
import {ApiDeck, ApiDeckAttributes} from '../../model/api/api.deck';
import {ApiSlide} from '../../model/api/api.slide';
import {Slide, SlideAttributes, SlideTemplate} from '../../model/data/slide';
import {Template} from '../../model/data/template';

import {findSlide} from '../../utils/data/slide-utils';
import {getGoogleFontScript} from './google-fonts-utils';
import {findTemplates, getTemplate} from '../../utils/data/template-utils';

interface SlideAndTemplate {
  apiSlide: ApiSlide;
  template: Template | undefined;
}

export function convertDeck(deck: Deck): Promise<ApiDeck> {
  return new Promise<ApiDeck>(async (resolve, reject) => {
    try {
      const apiSlides: SlideAndTemplate[] = await convertSlides(deck);

      const apiDeck: ApiDeck = {
        name: deck.data.name ? deck.data.name.trim() : deck.data.name,
        description:
          deck.data.meta && deck.data.meta.description !== undefined && deck.data.meta.description !== ''
            ? (deck.data.meta.description as string)
            : deck.data.name,
        owner_id: deck.data.owner_id,
        background: deck.data.background,
        header: deck.data.header,
        footer: deck.data.footer,
        slides: apiSlides.map((slideAndTemplate: SlideAndTemplate) => slideAndTemplate.apiSlide),
      };

      const googleFontScript: string | undefined = await getGoogleFontScript(deck);
      if (googleFontScript !== undefined) {
        apiDeck.head_extra = googleFontScript;
      }

      const scripts: string | undefined = getTemplateScripts(apiSlides);

      if (scripts !== undefined) {
        apiDeck.head_extra = apiDeck.head_extra !== undefined ? `${apiDeck.head_extra}${scripts}` : scripts;
      }

      const attributes: ApiDeckAttributes | undefined = await convertDeckAttributes(deck);
      if (attributes !== undefined) {
        apiDeck.attributes = attributes;
      }

      resolve(apiDeck);
    } catch (err) {
      reject(err);
    }
  });
}

async function convertDeckAttributes(deck: Deck): Promise<ApiDeckAttributes | undefined> {
  if (!deck || !deck.data || !deck.data.attributes) {
    return undefined;
  }

  const attributes: ApiDeckAttributes = {};

  if (deck.data.attributes.style) {
    attributes.style = deck.data.attributes.style;
  }

  if (deck.data.attributes.animation) {
    attributes.animation = deck.data.attributes.animation;
  }

  if (deck.data.attributes.direction) {
    attributes.direction = deck.data.attributes.direction;
  }

  if (deck.data.attributes.directionMobile) {
    attributes['direction-mobile'] = deck.data.attributes.directionMobile;
  }

  if (deck.data.attributes.autoSlide) {
    attributes['auto-slide'] = 'true';
  }

  return attributes;
}

function convertSlides(deck: Deck): Promise<SlideAndTemplate[]> {
  return new Promise<SlideAndTemplate[]>(async (resolve, reject) => {
    if (!deck.data.slides || deck.data.slides.length <= 0) {
      resolve([]);
      return;
    }

    try {
      const templates: Template[] = await findTemplates(deck.data.owner_id);

      const promises: Promise<SlideAndTemplate>[] = [];

      for (let i: number = 0; i < deck.data.slides.length; i++) {
        const slideId: string = deck.data.slides[i];

        promises.push(convertSlide(deck, templates, slideId));
      }

      if (!promises || promises.length <= 0) {
        resolve([]);
        return;
      }

      const slides: SlideAndTemplate[] = await Promise.all(promises);

      resolve(slides);
    } catch (err) {
      reject(err);
    }
  });
}

function convertSlide(deck: Deck, templates: Template[], slideId: string): Promise<SlideAndTemplate> {
  return new Promise<SlideAndTemplate>(async (resolve, reject) => {
    const slide: Slide = await findSlide(deck.id, slideId);

    if (!slide || !slide.data) {
      reject('Missing slide for publishing');
      return;
    }

    const attributes: SlideAttributes | undefined = await convertAttributesToString(slide.data.attributes);

    const slideTemplate: SlideTemplate | undefined = SlideTemplate[slide.data.template.toUpperCase() as keyof typeof SlideTemplate];
    const slideTag: string = slideTemplate ? `deckgo-slide-${slideTemplate.toLowerCase()}` : slide.data.template;

    const apiSlide: ApiSlide = {
      template: slideTag,
      content: slide.data.content,
      attributes: attributes,
    };

    const cleanApiSlide: ApiSlide = await convertSlideQRCode(apiSlide);
    cleanApiSlide.content = await cleanNotes(apiSlide.content);

    const template: Template | undefined = await getTemplate(templates, slide.data.scope, slide.data.template);

    resolve({
      apiSlide: cleanApiSlide,
      template,
    });
  });
}

function convertAttributesToString(attributes: SlideAttributes | undefined): Promise<SlideAttributes | undefined> {
  return new Promise<SlideAttributes | undefined>((resolve) => {
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

function cleanNotes(content: string | undefined): Promise<string | undefined> {
  return new Promise<string | undefined>((resolve) => {
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

function getTemplateScripts(apiSlides: SlideAndTemplate[]) {
  const cdns: string[] | undefined = apiSlides
    .filter((slideAndTemplate: SlideAndTemplate) => slideAndTemplate.template !== undefined && slideAndTemplate.template.data.cdn !== undefined)
    .map((slideAndTemplate: SlideAndTemplate) => (slideAndTemplate.template as Template).data.cdn as string);

  if (cdns !== undefined && cdns.length > 0) {
    const uniqueCdns: string[] = [...new Set(cdns)];
    return uniqueCdns.map((cdn: string) => `<script type="module" src="${cdn}" />`).join();
  }

  return undefined;
}
