import {Config} from '@stencil/core';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
import autoprefixer from 'autoprefixer';

export const config: Config = {
  namespace: 'deckdeckgo-reveal',
  outputTargets: [
    {
      type: 'dist',
    },
    {
      type: 'www',
      serviceWorker: null,
    },
  ],
  plugins: [
    sass(),
    postcss({
      plugins: [autoprefixer()],
    }),
  ],
  devServer: {
    openBrowser: false,
    port: 3333,
  },
};
