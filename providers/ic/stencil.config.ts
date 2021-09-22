import {Config} from '@stencil/core';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
// @ts-ignore
import autoprefixer from 'autoprefixer';
import nodePolyfills from 'rollup-plugin-node-polyfills';
import replace from '@rollup/plugin-replace';

import {canisterEnvIds} from './dfx.config';

const dev: boolean = process.argv && process.argv.indexOf('--dev') > -1;
const prod = !dev;

export const config: Config = {
  namespace: 'deckdeckgo-ic',
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
    replace(canisterEnvIds(prod)),
    sass(),
    postcss({
      plugins: [autoprefixer()]
    })
  ],
  rollupPlugins: {
    after: [nodePolyfills()]
  },
  devServer: {
    openBrowser: false,
    port: 3335
  }
};
