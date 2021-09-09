import {Config} from '@stencil/core';

// @ts-ignore
import path from 'path';

const autoprefixer = require('autoprefixer');
import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
import replace from '@rollup/plugin-replace';
import nodePolyfills from 'rollup-plugin-node-polyfills';

import {canisterEnvIds} from './dfx.config';

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
  bundles: [
    {
      components: [
        'app-root',
        'app-user-delete',
        'app-deck-delete',
        'app-template',
        'app-notes',
        'app-remote-connect',
        'app-waves',
        'app-gif',
        'app-playground',
        'app-youtube',
        'app-photo',
        'app-poll-options',
        'app-code-languages',
        'app-custom-data',
        'app-demo',
        'app-publish',
        'app-embed',
        'app-custom-images',
        'app-user-menu',
        'app-deck-import',
        'app-image-element',
        'app-present',
        'app-edit-slide-chart',
        'app-edit-slide-user',
        'app-edit-slide-qrcode',
        'app-edit-slide-author',
        'app-edit-slide',
        'app-create-slide',
        'app-edit-mode',
        'app-element-style',
        'app-deck-style',
        'app-remote-request',
        'app-shape',
        'app-more-deck-actions',
        'app-more-element-actions',
        'app-more-share-options',
        'app-copy-style',
        'app-math',
        'app-get-help',
        'app-code',
        'app-transform-element',
        'app-transform-slide',
        'app-slide-warning-info',
        'app-element-delete',
        'app-slide-navigate',
        'app-fullscreen-info',
        'app-navigation',
        'app-share-deck',
        'app-menu',
        'app-navigation-actions',
        'app-logo',
        'app-spinner',
        'app-avatar',
        'app-user-info',
        'app-signin',
        'app-random-gif',
        'app-start-deck',
        'app-unpublish',
        'app-template-showcase',
        'app-links',
        'app-dashboard-deck-actions',
        'app-dashboard',
        'app-close-menu',
        'app-slide-warning',
        'app-slide-preview',
        'app-background-folders',
        'app-image-columns',
        'app-select-target-element',
        'app-share-options',
        'app-playground-placeholder',
        'app-color-chart',
        'app-color-qrcode',
        'app-color-sides',
        'app-image-style',
        'app-list',
        'app-border-radius',
        'app-color-word-cloud',
        'app-box-shadow',
        'app-block',
        'app-text',
        'app-color-code',
        'app-reveal',
        'app-expansion-panel',
        'app-color-text-background',
        'app-color',
        'app-image',
        'app-deck-fonts',
        'app-deck-transition',
        'app-deck-header-footer',
        'app-publish-tags',
        'app-publish-edit',
        'app-publish-done',
        'app-slot-type',
        'app-bottom-sheet',
        'app-actions-editor',
        'app-actions-element',
        'app-breadcrumbs',
        'app-action-help',
        'app-actions-deck',
        'app-action-share',
        'app-action-busy',
        'app-action-add-slide',
        'app-templates-charts',
        'app-templates-default',
        'app-templates-content',
        'app-templates-title',
        'app-templates-split',
        'app-no-templates',
        'app-templates-community',
        'app-templates-user',
        'app-templates',
        'app-profile',
        'app-customization',
        'app-poll',
        'app-signin-page',
        'app-dashboard-page',
        'app-editor'
      ]
    }
  ],
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
    replace(canisterEnvIds(prod)),
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
    after: [nodePolyfills()]
  }
};
