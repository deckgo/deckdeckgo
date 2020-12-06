import {Config} from '@stencil/core';
import {JsonDocs} from '@stencil/core/internal';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
import autoprefixer from 'autoprefixer';

import {generateDesc} from './deckdeckgo.desc';

export const config: Config = {
  namespace: 'deckdeckgo-slide-countdown',
  outputTargets: [
    {
      type: 'dist',
    },
    {
      type: 'www',
      serviceWorker: null,
    },
    {
      type: 'docs-custom',
      generator: (docs: JsonDocs) => {
        generateDesc(docs);
      },
    },
  ],
  plugins: [
    sass({
      includePaths: ['node_modules/@deckdeckgo/slide-utils/styles/'],
    }),
    postcss({
      plugins: [autoprefixer()],
    }),
  ],
};
