import { enableInputBlurring } from '../app/utils/ionic/ionic.input-blurring';
enableInputBlurring();

import {setupConfig as setupDeckGoConfig} from '../app/types/core/environment-config';

setupDeckGoConfig({
  deckdeckgo: {
    prod: false,
    appUrl: 'https://iey7l-kaaaa-aaaah-qadoa-cai.raw.ic0.app',
    globalAssetsUrl: 'https://iey7l-kaaaa-aaaah-qadoa-cai.raw.ic0.app/assets',
    pollUrl: 'https://iey7l-kaaaa-aaaah-qadoa-cai.raw.ic0.app/poll',
    apiUrl: '<@API_URL@>',
    presentationUrl: '<@PRESENTATION_URL@>',
    socketUrl: '<@SOCKET_URL@>'
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
    appId: '<@FIREBASE_APP_ID@>'
  },
  tenor: {
    url: 'https://api.tenor.com/v1/',
    key: '<@TENOR_KEY@>'
  },
  unsplash: {
    url: '<@UNSPLASH_URL@>'
  },
  google: {
    fontsUrl: 'https://fonts.googleapis.com/css?display=swap&family='
  }
});

// IE9: https://stackoverflow.com/questions/5472938/does-ie9-support-console-log-and-is-it-a-real-function#answer-5473193
const log: any = Function.prototype.bind.call(console.log, console);
log.apply(console, ['%cDeckDeckGo', 'color: #3880ff;font-size:2rem;font-weight: 300;']);
log.apply(console, ['%chttps://github.com/deckgo/deckdeckgo', 'font-size:1rem;font-weight: 300;']);
