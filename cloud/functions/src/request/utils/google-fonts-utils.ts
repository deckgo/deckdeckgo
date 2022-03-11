import {JSDOM} from 'jsdom';
import {Deck} from '../../model/data/deck';

// TODO: Extract to utilities because it is also use in studio

export interface GoogleFont {
  id: string;
  name: string;
  family: string;
}

// TODO fetch(`https://app.deckdeckgo.com/assests/assets.json`) at build time
export const fonts: GoogleFont[] = [
  {
    id: 'google-fonts-lora',
    name: 'Lora',
    family: "'Lora', serif",
  },
  {
    id: 'google-fonts-roboto',
    name: 'Roboto',
    family: "'Roboto', sans-serif",
  },
  {
    id: 'google-fonts-open-sans',
    name: 'Open Sans',
    family: "'Open Sans', sans-serif",
  },
  {
    id: 'google-fonts-montserrat',
    name: 'Montserrat',
    family: "'Montserrat', sans-serif",
  },
  {
    id: 'google-fonts-cabin',
    name: 'Cabin',
    family: "'Cabin', sans-serif",
  },
  {
    id: 'google-fonts-lato',
    name: 'Lato',
    family: "'Lato', sans-serif",
  },
  {
    id: 'google-fonts-muli',
    name: 'Muli',
    family: "'Muli', sans-serif",
  },
  {
    id: 'google-fonts-source-sans-pro',
    name: 'Source Sans Pro',
    family: "'Source Sans Pro', sans-serif",
  },
  {
    id: 'google-fonts-libre-baskerville',
    name: 'Libre Baskerville',
    family: "'Libre Baskerville', serif",
  },
  {
    id: 'google-fonts-oswald',
    name: 'Oswald',
    family: "'Oswald', sans-serif",
  },
  {
    id: 'google-fonts-jura',
    name: 'Jura',
    family: "'Jura', sans-serif",
  },
  {
    id: 'google-fonts-fjord-one',
    name: 'Fjord One',
    family: "'Fjord One', serif",
  },
  {
    id: 'google-fonts-josefin-slab',
    name: 'Josefin Slab',
    family: "'Josefin Slab', serif",
  },
];

const googleFontsUrl: string = 'https://fonts.googleapis.com/css?display=swap&amp;family=';

export function getGoogleFontScript(deck: Deck): Promise<string | undefined> {
  return new Promise<string | undefined>(async (resolve) => {
    if (!deck || !deck.data || !deck.data.attributes) {
      resolve(undefined);
      return;
    }

    if (!deck.data.attributes.style || deck.data.attributes.style === undefined || deck.data.attributes.style === '') {
      resolve(undefined);
      return;
    }

    const dom = new JSDOM(`<!DOCTYPE html><div></div>`);
    const div = dom.window.document.querySelector('div');

    if (!div) {
      return;
    }

    div.setAttribute('style', deck.data.attributes.style);

    const fontFamily: string | undefined = div.style.getPropertyValue('font-family');

    if (!fontFamily || fontFamily === undefined || fontFamily === '') {
      resolve(undefined);
      return;
    }

    const font: GoogleFont | undefined = await extractGoogleFont(fontFamily);

    if (!font || font === undefined) {
      resolve(undefined);
      return;
    }

    const url: string = getGoogleFontUrl(googleFontsUrl, font);

    const link: string = `<link rel="stylesheet" href="${url}">`;

    resolve(link);
  });
}

async function extractGoogleFont(fontFamilyStyle: string): Promise<GoogleFont | undefined> {
  if (!fontFamilyStyle || fontFamilyStyle === undefined) {
    return undefined;
  }

  const fontFamily: string = fontFamilyStyle.replace(/\'/g, '').replace(/"/g, '');

  const font: GoogleFont | undefined = fonts.find((filteredFont: GoogleFont) => {
    return fontFamily === filteredFont.family.replace(/\'/g, '');
  });

  return font;
}

function getGoogleFontUrl(fontsUrl: string, font: GoogleFont): string {
  return fontsUrl + font.name.replace(' ', '+');
}
