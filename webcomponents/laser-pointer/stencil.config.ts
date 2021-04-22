import {Config} from '@stencil/core';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
import autoprefixer from 'autoprefixer';

export const config: Config = {
  namespace: 'deckdeckgo-laser-pointer',
  outputTargets: [
    {
      type: 'dist',
    },
    {
      type: 'www',
      serviceWorker: null,
    },
    {
      type: 'docs-readme'
    }
  ],
  plugins: [
    sass(),
    postcss({
      plugins: [autoprefixer()],
    }),
  ],
};
