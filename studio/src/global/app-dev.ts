import '@ionic/core';

import {setupConfig} from '@ionic/core';
setupConfig({
  inputBlurring: false
});

import '@deckdeckgo/core';

import '@deckdeckgo/inline-editor';
import '@deckdeckgo/remote';
import '@deckdeckgo/qrcode';
import '@deckdeckgo/highlight-code';
import '@deckdeckgo/lazy-img';
import '@deckdeckgo/color';
import '@deckdeckgo/charts';
import '@deckdeckgo/social';
import '@deckdeckgo/youtube';
import '@deckdeckgo/drag-resize-rotate';

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

import {setupConfig as setupDeckGoConfig} from '../app/services/core/environment/environment-config';

setupDeckGoConfig({
  deckdeckgo: {
    prod: false,
    appUrl: 'http://localhost:3333',
    globalAssetsUrl: 'https://deckdeckgo-studio-beta.web.app/assets',
    pollUrl: 'https://localhost:3333/poll',
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
    appId: '<@FIREBASE_APP_ID@>'
  },
  tenor: {
    url: 'https://api.tenor.com/v1/',
    key: '<@TENOR_KEY@>'
  },
  unsplash: {
    url: '<@UNSPLASH_URL@>'
  },
  prismComponentsUrl: 'https://raw.githubusercontent.com/PrismJS/prism/886698d5b759ef46162a5723a2493f97c689dc94/components.json',
  gifExampleSrc: 'https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/200w_d.gif',
  google: {
    fontsUrl: 'https://fonts.googleapis.com/css?display=swap&family='
  }
});

// https://github.com/deckgo/deckdeckgo/issues/327
// https://github.com/ionic-team/ionic/issues/19065

const hack = () => {
  const ionApp = document.querySelector('ion-app');

  if (ionApp) {
    window.requestAnimationFrame(() => {
      ionApp.style.height = '100%';
      window.requestAnimationFrame(() => {
        ionApp.style.height = '';
      });
    });
  }
};

let resizerObserver;

document.addEventListener('DOMContentLoaded', () => {
  if (!window) {
    return;
  }

  if ('ResizeObserver' in window) {
    const ResizeObserver = (window as any).ResizeObserver;
    resizerObserver = new ResizeObserver(hack);
    resizerObserver.observe(document.documentElement);
  } else {
    window.addEventListener('keyboardWillShow', hack);
    window.addEventListener('keyboardWillHide', hack);
    window.addEventListener('resize', hack);
  }
});

window.addEventListener('unload', () => {
  if (!window) {
    return;
  }

  if ('ResizeObserver' in window) {
    if (resizerObserver) {
      resizerObserver.unobserve(document.documentElement);
      resizerObserver.disconnect();
    }
  } else {
    window.removeEventListener('keyboardWillShow', hack);
    window.removeEventListener('keyboardWillHide', hack);
    window.removeEventListener('resize', hack);
  }
});
