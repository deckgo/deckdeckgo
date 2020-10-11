import '@ionic/core';

import '@deckdeckgo/core';
import '@deckdeckgo/highlight-code';
import '@deckdeckgo/lazy-img';
import '@deckdeckgo/qrcode';
import '@deckdeckgo/charts';
import '@deckdeckgo/social';
import '@deckdeckgo/youtube';
import '@deckdeckgo/drag-resize-rotate';
import '@deckdeckgo/demo';
import '@deckdeckgo/math';
import '@deckdeckgo/word-cloud';

import '@deckdeckgo/slide-title';
import '@deckdeckgo/slide-author';
import '@deckdeckgo/slide-code';
import '@deckdeckgo/slide-chart';
import '@deckdeckgo/slide-split';
import '@deckdeckgo/slide-qrcode';
import '@deckdeckgo/slide-content';
import '@deckdeckgo/slide-gif';
import '@deckdeckgo/slide-countdown';
import '@deckdeckgo/slide-youtube';
import '@deckdeckgo/slide-big-img';
import '@deckdeckgo/slide-video';
import '@deckdeckgo/slide-poll';
import '@deckdeckgo/slide-aspect-ratio';
import '@deckdeckgo/slide-playground';

import {setupConfig} from '../app/services/environment/environment-config';

setupConfig({
  signalingServerUrl: 'https://api.deckdeckgo.com',
});

// import { setupConfig } from '@ionic/core';

// setupConfig({
//   mode: 'ios'
// });
