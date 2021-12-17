import {Config} from '@stencil/core';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
// @ts-ignore
import autoprefixer from 'autoprefixer';

export const config: Config = {
  namespace: 'deckdeckgo-firebase',
  outputTargets: [
    {
      type: 'dist'
    },
    {
      type: 'www',
      serviceWorker: null
    },
    {
      type: 'dist-custom-elements'
    }
  ],
  plugins: [
    sass(),
    postcss({
      plugins: [autoprefixer()]
    })
  ],
  devServer: {
    openBrowser: false,
    port: 3335
  }
};
