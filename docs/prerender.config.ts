import {PrerenderConfig} from '@stencil/core';
import {PrerenderHydrateOptions} from '@stencil/core/internal/stencil-public-compiler';

export const config: PrerenderConfig = {
  hydrateOptions(url?: URL): PrerenderHydrateOptions {
    const hydrate: PrerenderHydrateOptions = {
      excludeComponents: ['deckgo-highlight-code', 'deckgo-slide-poll', 'deckgo-word-cloud'],
    };
    return hydrate;
  },
};
