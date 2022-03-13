import {Config} from '@stencil/core';
import {postcss} from '@stencil/postcss';
import {sass} from '@stencil/sass';
// @ts-ignore
import autoprefixer from 'autoprefixer';
import nodePolyfills from 'rollup-plugin-node-polyfills';

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
      type: 'dist-custom-elements'
    }
  ],
  plugins: [
    sass(),
    postcss({
      plugins: [autoprefixer()]
    })
  ],
  rollupPlugins: {
    after: [
      nodePolyfills({
        include: ['node_modules/**/*.js', '../../node_modules/**/*.js']
      })
    ]
  },
  devServer: {
    openBrowser: false,
    port: 3335
  }
};
