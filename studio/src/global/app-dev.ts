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
    mock: true
  },
  deckdeckgo: {
    website: 'https://deckdeckgo.com',
    globalAssetsUrl: 'http://localhost:3333/assets',
    pollUrl: 'http://localhost:3333/poll',
    socketUrl: '<@SOCKET_URL@>'
  },
  google: {
    fontsUrl: 'https://fonts.googleapis.com/css?display=swap&family='
  }
});
