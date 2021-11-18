import {Deck, DeckData, deckSelector, getGoogleFontUrl, GoogleFont, googleFonts, Publish} from '@deckdeckgo/editor';

import {setData} from '../../utils/data.utils';

interface ApiDeckAttributes {
  style?: string;
  animation?: string;
  direction?: string;
  'direction-mobile'?: string;
  'auto-slide'?: string;
}

interface ApiDeck {
  id?: string;
  slides: ApiSlide[];
  name: string;
  description: string;
  owner_id: string;
  attributes?: ApiDeckAttributes;
  background?: string;
  header?: string;
  footer?: string;
  head_extra?: string;
}

export const publish: Publish = async ({deck: deckSource}: {deck: Deck; config: Record<string, string>}): Promise<Deck> => {
  const {id, data} = deckSource;

  // 1. Update deck meta information
  const deck: Deck = await setData<Deck, DeckData>({key: `/decks/${id}`, id, data});

  // 2. Trigger the function that effectively publish
  await publishDeck({deck});

  return deck;
};

const publishDeck = async ({deck}: {deck: Deck}): Promise<void> => {
  const googleFontScript: string | undefined = getGoogleFontScript();

  const apiDeck: ApiDeck = {
    name: deck.data.name?.trim(),
    description: deck.data.meta.description,
    background: deck.data.background,
    header: deck.data.header,
    footer: deck.data.footer,
    slides: apiSlides.map((slideAndTemplate: SlideAndTemplate) => slideAndTemplate.apiSlide),
    ...(googleFontScript && {head_extra: googleFontScript}),
    attributes: getAttributes()
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

const getAttributes = (): Record<string, string> => {
  const deck: HTMLElement | null = document.querySelector(deckSelector);
  const attr: NamedNodeMap = deck?.attributes;

  return Array.from(attr)
    .filter(({name}: Attr) => !['id', 'hydrated', 'class', 'contenteditable'].includes(name))
    .reduce((acc, {name, value}: Attr) => {
      acc[name] = value;
      return acc;
    }, {} as Record<string, string>);
};
