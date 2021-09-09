import {Config} from '@stencil/core';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
// @ts-ignore
import autoprefixer from 'autoprefixer';

import builtins from 'rollup-plugin-node-builtins';

export const config: Config = {
  namespace: 'deckdeckgo-remote',
  outputTargets: [
    {
      type: 'dist'
    },
    {
      type: 'www',
      serviceWorker: null
    },
    {
      type: 'dist-custom-elements-bundle'
    }
  ],
  plugins: [
    sass(),
    postcss({
      plugins: [autoprefixer()]
    }),
    builtins()
  ],
  nodeResolve: {
    browser: true
  }
};
