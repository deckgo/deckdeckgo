import '@ionic/core';

import {setupConfig} from '@ionic/core';
setupConfig({
  inputBlurring: false,
});

import '@deckdeckgo/core';

import '@deckdeckgo/inline-editor';
import '@deckdeckgo/remote';
import '@deckdeckgo/qrcode';
import '@deckdeckgo/highlight-code';
import '@deckdeckgo/math';
import '@deckdeckgo/lazy-img';
import '@deckdeckgo/color';
import '@deckdeckgo/charts';
import '@deckdeckgo/social';
import '@deckdeckgo/reveal';
import '@deckdeckgo/youtube';
import '@deckdeckgo/drag-resize-rotate';
import '@deckdeckgo/demo';
import '@deckdeckgo/word-cloud';
import '@deckdeckgo/markdown';

import '@deckdeckgo/slide-title';
import '@deckdeckgo/slide-content';
import '@deckdeckgo/slide-split';
import '@deckdeckgo/slide-gif';
import '@deckdeckgo/slide-youtube';
import '@deckdeckgo/slide-author';
import '@deckdeckgo/slide-qrcode';
import '@deckdeckgo/slide-chart';
import '@deckdeckgo/slide-poll';
import '@deckdeckgo/slide-aspect-ratio';
import '@deckdeckgo/slide-playground';
import '@deckdeckgo/slide-svg';

import {setupConfig as setupDeckGoConfig} from '../app/types/core/environment-config';

setupDeckGoConfig({
  deckdeckgo: {
    prod: false,
    appUrl: 'https://deckdeckgo-studio-staging.web.app',
    globalAssetsUrl: 'https://deckdeckgo-studio-staging.web.app/assets',
    pollUrl: 'https://deckdeckgo-studio-staging.web.app/poll',
    apiUrl: '<@API_URL@>',
    presentationUrl: '<@PRESENTATION_URL@>',
    socketUrl: '<@SOCKET_URL@>',
  },
  firebase: {
    apiKey: '<@FIREBASE_API_KEY@>',
    authDomain: '<@FIREBASE_AUTH_DOMAIN@>',
    databaseURL: '<@FIREBASE_DATABASE_URL@>',
    projectId: '<@FIREBASE_PROJECT_ID@>',
    storageBucket: '<@FIREBASE_STORAGE_BUCKET@>',
    messagingSenderId: '<@FIREBASE_MESSAGING_SENDER_ID@>',
    storageUrl: '<@FIREBASE_STORAGE_URL@>',
    functionsUrl: '<@FIREBASE_FUNCTIONS_URL@>',
    appId: '<@FIREBASE_APP_ID@>',
  },
  tenor: {
    url: 'https://api.tenor.com/v1/',
    key: '<@TENOR_KEY@>',
  },
  unsplash: {
    url: '<@UNSPLASH_URL@>',
  },
  google: {
    fontsUrl: 'https://fonts.googleapis.com/css?display=swap&family=',
  },
});
