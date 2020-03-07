import {Config} from '@stencil/core';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
import autoprefixer from 'autoprefixer';

import replace from '@rollup/plugin-replace';

// @ts-ignore
const dev: boolean = process.argv && process.argv.indexOf('--dev') > -1;
// @ts-ignore
const staging: boolean = process.argv && process.argv.indexOf('--staging') > -1;

const globalScript: string = dev ? 'src/global/app-dev.ts' : staging ? 'src/global/app-staging.ts' : 'src/global/app.ts';

const configDataFile = dev ? './config.dev.json' : staging ? './config.staging.json' : './config.prod.json';
const configValues = require(configDataFile);

export const config: Config = {
  outputTargets: [
    {
      type: 'www',
      baseUrl: 'https://deckdeckgo.com',
      serviceWorker: {
        swSrc: 'src/sw.js'
      }
    }
  ],
  globalScript: globalScript,
  globalStyle: 'src/global/app.scss',
  plugins: [
    replace({
      exclude: 'node_modules/**',
      delimiters: ['<@', '@>'],
      values: configValues
    }),
    sass({
      includePaths: ['node_modules/@deckdeckgo/deck-utils/styles/']
    }),
    postcss({
      plugins: [autoprefixer()]
    })
  ],
  nodeResolve: {browser: true},
  devServer: {
    openBrowser: false,
    reloadStrategy: 'pageReload'
  },
  copy: [{src: 'robots.txt'}]
};
