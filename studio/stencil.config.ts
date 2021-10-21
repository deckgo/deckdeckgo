import {Config} from '@stencil/core';

// @ts-ignore
import path from 'path';

const autoprefixer = require('autoprefixer');
import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
import replace from '@rollup/plugin-replace';

// @ts-ignore
const dev: boolean = process.argv && process.argv.indexOf('--dev') > -1;
// @ts-ignore
const staging: boolean = process.argv && process.argv.indexOf('--staging') > -1;
// @ts-ignore
const internetComputer: boolean = process.argv && process.argv.indexOf('--ic') > -1;

const prod = !(dev || staging || internetComputer);

const globalScript: string =
  dev && !staging && !internetComputer
    ? 'src/global/app-dev.ts'
    : staging
    ? 'src/global/app-staging.ts'
    : internetComputer
    ? 'src/global/app-ic.ts'
    : 'src/global/app.ts';

const configDataFile = dev && !staging ? './config.dev.json' : staging || internetComputer ? './config.staging.json' : './config.prod.json';
const configValues = require(configDataFile);

const assetLinks = !prod ? 'assetlinks.dev.json' : 'assetlinks.prod.json';

export const config: Config = {
  outputTargets: [
    {
      type: 'www',
      baseUrl: 'https://deckdeckgo.com',
      serviceWorker: {
        swSrc: 'src/sw.js'
      },
      copy: [{src: 'robots.txt'}, {src: `${assetLinks}`, dest: `.well-known/assetlinks.json`}]
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
  }
};
