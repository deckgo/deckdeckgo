import '@ionic/core';

import {setupConfig} from '@ionic/core';
setupConfig({
  inputBlurring: false
});

import '@deckdeckgo/core';

import '@deckdeckgo/inline-editor';
import '@deckdeckgo/remote';
import '@deckdeckgo/highlight-code';
import '@deckdeckgo/math';
import '@deckdeckgo/lazy-img';
import '@deckdeckgo/color';
import '@deckdeckgo/charts';
import '@deckdeckgo/drag-resize-rotate';
import '@deckdeckgo/word-cloud';
import '@deckdeckgo/markdown';
import '@deckdeckgo/laser-pointer';
import '@deckdeckgo/elements';
import '@deckdeckgo/social-img';

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

import {setupConfig as setupDeckGoConfig} from '../app/types/core/environment-config';

setupDeckGoConfig({
  app: {
    mock: false
  },
  deckdeckgo: {
    website: 'https://deckdeckgo.com',
    globalAssetsUrl: 'https://iey7l-kaaaa-aaaah-qadoa-cai.raw.ic0.app/assets',
    pollUrl: 'https://iey7l-kaaaa-aaaah-qadoa-cai.raw.ic0.app/poll',
    apiUrl: '<@API_URL@>',
    socketUrl: '<@SOCKET_URL@>'
  },
  tenor: {
    url: 'https://api.tenor.com/v1/',
    key: '<@TENOR_KEY@>'
  },
  unsplash: {
    url: '<@UNSPLASH_URL@>',
    cdn: 'https://unpkg.com/@deckdeckgo/api@latest/lib/index.js'
  },
  google: {
    fontsUrl: 'https://fonts.googleapis.com/css?display=swap&family='
  },
  cloud: {
    api: {
      cdn: 'http://localhost:3335/build/index.esm.js'
    },
    signIn: {
      cdn: 'http://localhost:3335/build/deckdeckgo-ic.esm.js',
      tag: 'deckgo-ic-signin'
    }
  }
});
