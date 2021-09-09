import {Config} from '@stencil/core';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
// @ts-ignore
import autoprefixer from 'autoprefixer';

export const config: Config = {
  namespace: 'deckdeckgo-slide-aspect-ratio',
  outputTargets: [
    {
      type: 'dist',
    },
    {
      type: 'www',
      serviceWorker: null,
    },
    {
      type: 'docs-readme',
    },
    {
      type: 'dist-custom-elements-bundle',
    },
  ],
  devServer: {
    openBrowser: false,
    port: 3334,
  },
  plugins: [
    sass({
      includePaths: ['../../node_modules/@deckdeckgo/slide-utils/styles/'],
    }),
    postcss({
      plugins: [autoprefixer()],
    }),
  ],
};
