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
    mock: false,
    features: ['doc']
  },
  deckdeckgo: {
    website: 'https://deckdeckgo.com',
    globalAssetsUrl: 'https://iey7l-kaaaa-aaaah-qadoa-cai.raw.ic0.app/assets',
    pollUrl: 'https://iey7l-kaaaa-aaaah-qadoa-cai.raw.ic0.app/poll',
    apiUrl: '<@API_URL@>',
    socketUrl: '<@SOCKET_URL@>',
    terms: 'https://deckdeckgo.com/terms',
    privacy: 'https://deckdeckgo.com/privacy'
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
      cdn: 'https://unpkg.com/@deckdeckgo/ic@5.3.5/dist/deckdeckgo-ic/index.esm.js'
    },
    signIn: {
      cdn: 'https://unpkg.com/@deckdeckgo/ic@5.3.5/dist/deckdeckgo-ic/deckdeckgo-ic.esm.js',
      tag: 'deckgo-ic-signin'
    }
  },
  jszip: {
    cdn: 'https://cdn.jsdelivr.net/npm/jszip@3.7.1/dist/jszip.min.js'
  },
  ic: {
    managerCanisterId: 'jkpvl-ziaaa-aaaai-aaafq-cai'
  }
});

// IE9: https://stackoverflow.com/questions/5472938/does-ie9-support-console-log-and-is-it-a-real-function#answer-5473193
const log: any = Function.prototype.bind.call(console.log, console);
log.apply(console, ['%cDeckDeckGo', 'color: #3880ff;font-size:2rem;font-weight: 300;']);
log.apply(console, ['%chttps://github.com/deckgo/deckdeckgo', 'font-size:1rem;font-weight: 300;']);
