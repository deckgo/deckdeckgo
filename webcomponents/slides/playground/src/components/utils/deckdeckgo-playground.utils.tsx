import {isMobile} from '@deckdeckgo/utils';

import {DeckdeckgoPlaygroundTheme} from '../..';

export async function formatPlaygroundSrc(src: string | undefined, theme: DeckdeckgoPlaygroundTheme): Promise<string | undefined> {
  if (!src) {
    return undefined;
  }

  const url: URL = new URL(src);

  if (!url.hostname) {
    return undefined;
  }

  if (url.hostname.match(/codepen\.[\s\S]*/)) {
    return formatCodepenSrc(src, theme);
  } else if (url.hostname.match(/jsfiddle\.[\s\S]*/)) {
    return formatJSFiddleSrc(src, theme);
  } else if (url.hostname.match(/webcomponents\.[\s\S]*/)) {
    return formatWebComponentsDevSrc(src);
  }

  return undefined;
}

async function formatCodepenSrc(src: string, theme: DeckdeckgoPlaygroundTheme): Promise<string | undefined> {
  const embedSrc: string = await formatEmbedCodepenSrc(src);
  return `${embedSrc}?default-tab=result&embed-version=2&theme-id=${theme.toLowerCase()}`;
}

async function formatJSFiddleSrc(src: string, theme: DeckdeckgoPlaygroundTheme): Promise<string | undefined> {
  return `${src}embedded/js,html,css,result/${
    theme !== DeckdeckgoPlaygroundTheme.DEFAULT ? theme.toLowerCase() : DeckdeckgoPlaygroundTheme.DARK.toLowerCase()
  }`;
}

async function formatEmbedCodepenSrc(src: string): Promise<string> {
  // On mobile device, embed pens in a preview state where they need to be clicked to loaded
  if (isMobile()) {
    return src.replace('/pen/', '/embed/preview/');
  } else {
    return src.replace('/pen/', '/embed/');
  }
}

async function formatWebComponentsDevSrc(src: string): Promise<string | undefined> {
  return `${src.replace('/preview/', '/v/')}?embed=1&sv=1&pm=1`;
}
