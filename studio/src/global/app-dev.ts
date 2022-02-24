import '@deckdeckgo/charts';
import '@deckdeckgo/core';
import '@deckdeckgo/drag-resize-rotate';
import '@deckdeckgo/elements';
import '@deckdeckgo/highlight-code';
import '@deckdeckgo/laser-pointer';
import '@deckdeckgo/lazy-img';
import '@deckdeckgo/markdown';
import '@deckdeckgo/math';
import '@deckdeckgo/remote';
import '@deckdeckgo/slide-aspect-ratio';
import '@deckdeckgo/slide-author';
import '@deckdeckgo/slide-chart';
import '@deckdeckgo/slide-content';
import '@deckdeckgo/slide-gif';
import '@deckdeckgo/slide-playground';
import '@deckdeckgo/slide-poll';
import '@deckdeckgo/slide-qrcode';
import '@deckdeckgo/slide-split';
import '@deckdeckgo/slide-title';
import '@deckdeckgo/slide-youtube';
import '@deckdeckgo/social-img';
import '@deckdeckgo/studio';
import '@deckdeckgo/word-cloud';
import '@ionic/core';
import {setupConfig} from '@ionic/core';
import {setupConfig as setupDeckGoConfig} from '../app/config/environment-config';

setupConfig({
  inputBlurring: false
});

setupDeckGoConfig({
  app: {
    mock: true,
    features: ['deck', 'doc']
  },
  deckdeckgo: {
    website: 'https://deckdeckgo.com',
    globalAssetsUrl: 'http://localhost:3333/assets',
    pollUrl: 'http://localhost:3333/poll',
    socketUrl: '<@SOCKET_URL@>',
    terms: 'https://deckdeckgo.com/terms',
    privacy: 'https://deckdeckgo.com/privacy'
  },
  google: {
    fontsUrl: 'https://fonts.googleapis.com/css?display=swap&family='
  }
});
