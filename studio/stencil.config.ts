import {Config} from '@stencil/core';

// @ts-ignore
import path from 'path';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
const autoprefixer = require('autoprefixer');
import alias from '@rollup/plugin-alias';
import nodePolyfills from 'rollup-plugin-node-polyfills';

// @ts-ignore
import dfxJson from './dfx.json';

import replace from '@rollup/plugin-replace';

// @ts-ignore
const dev: boolean = process.argv && process.argv.indexOf('--dev') > -1;
// @ts-ignore
const staging: boolean = process.argv && process.argv.indexOf('--staging') > -1;

const globalScript: string = dev && !staging ? 'src/global/app-dev.ts' : staging ? 'src/global/app-staging.ts' : 'src/global/app.ts';

const configDataFile = dev && !staging ? './config.dev.json' : staging ? './config.staging.json' : './config.prod.json';
const configValues = require(configDataFile);

const assetLinks = dev || staging ? 'assetlinks.dev.json' : 'assetlinks.prod.json';

const dfxAliases = Object.entries(dfxJson.canisters)
  .filter(([_key, {type}]: [string, {type: 'motoko' | 'assets'}]) => type !== 'assets')
  .reduce((acc, [name, _value]) => {
    // Get the network name, or `local` by default.
    const networkName = process.env['DFX_NETWORK'] || 'local';
    const outputRoot = path.join(__dirname, '.dfx', networkName, 'canisters', name);

    return {
      ...acc,
      ['dfx-generated/' + name]: path.join(outputRoot, name + '.js')
    };
  }, {});

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
  },
  rollupPlugins: {
    before: [
      alias({
        entries: dfxAliases
      })
    ],
    after: [nodePolyfills()]
  }
};
