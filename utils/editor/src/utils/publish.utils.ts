import {Deck} from '../models/data/deck';

import {deckSelector} from './deck.utils';
import {getGoogleFontUrl, GoogleFont, googleFonts} from './fonts.utils';
import {cleanNode, isElementNode} from './node.utils';

export interface DeckPublishData {
  title: string;
  description: string;
  author: string;
  photo_url: string | undefined;
  slides: string[];
  background: string | undefined;
  header: string | undefined;
  footer: string | undefined;
  head_extra: string | undefined;
  attributes: Record<string, string> | undefined;
}

export const publishData = ({deck}: {deck: Deck}): DeckPublishData => {
  const googleFontScript: string | undefined = getGoogleFontScript();
  const attributes: Record<string, string> | undefined = getAttributes();

  const {data} = deck;
  const {meta, background, footer, header} = data;

  return {
    title: (meta?.title || data.name)?.trim(),
    description: (meta?.description || data.name)?.trim(),
    author: meta?.author?.name || 'DeckDeckGo',
    photo_url: meta?.author?.photo_url,
    slides: getSlides(),
    background: background ? `<div slot="background">${background}</div>` : undefined,
    header: background ? `<div slot="header">${header}</div>` : undefined,
    footer: background ? `<div slot="footer">${footer}</div>` : undefined,
    head_extra: googleFontScript,
    attributes
  };
};

const getGoogleFontScript = (): string | undefined => {
  const deck: HTMLElement | null = document.querySelector(deckSelector);
  const fontFamily: string | undefined = deck?.style.fontFamily;

  if (!fontFamily) {
    return undefined;
  }

  const font: GoogleFont | undefined = googleFonts.find((filteredFont: GoogleFont) => {
    return fontFamily === filteredFont.family.replace(/\'/g, '');
  });

  if (!font) {
    return undefined;
  }

  const url: string = getGoogleFontUrl({font});

  return `<link rel="stylesheet" href="${url}">`;
};

const getAttributes = (): Record<string, string> | undefined => {
  const deck: HTMLElement | null = document.querySelector(deckSelector);
  const attr: NamedNodeMap | undefined = deck?.attributes;

  if (!attr) {
    return undefined;
  }

  return Array.from(attr)
    .filter(({name}: Attr) => !['id', 'hydrated', 'class', 'contenteditable'].includes(name))
    .reduce((acc, {name, value}: Attr) => {
      acc[name] = value;
      return acc;
    }, {} as Record<string, string>);
};

const getSlides = (): string[] => {
  const slides: NodeListOf<HTMLElement> = document.querySelectorAll(`${deckSelector} > *[slide_id]`);

  const removeCustomSlot = ({slide, custom}: {slide: HTMLElement; custom: 'background' | 'header' | 'footer'}) => {
    if (slide.hasAttribute(`custom-${custom}`)) {
      return;
    }

    const slot: HTMLElement | null = slide.querySelector(`div[slot="${custom}"]`);

    if (!slot) {
      return;
    }

    slide.removeChild(slot);
  };

  const cloneSlides: HTMLElement[] = Array.from(slides).map((slide: HTMLElement) => {
    const cloneSlide: HTMLElement = cleanNode({node: slide, deep: false}) as HTMLElement;

    const nodes: Node[] = Array.from(slide.childNodes);
    nodes.forEach((node: Node) => {
      if (isElementNode(node) && (node as HTMLElement).hasAttribute('slot')) {
        cloneSlide.appendChild(cleanNode({node}) as Node);

        removeCustomSlot({slide: cloneSlide, custom: 'background'});
        removeCustomSlot({slide: cloneSlide, custom: 'header'});
        removeCustomSlot({slide: cloneSlide, custom: 'footer'});
      }
    });

    return cloneSlide;
  });

  return cloneSlides.map((clone: HTMLElement) => clone.outerHTML);
};
