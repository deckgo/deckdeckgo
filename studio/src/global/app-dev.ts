import {enableInputBlurring} from '../app/utils/ionic/ionic.input-blurring';
enableInputBlurring();

import {setupConfig as setupDeckGoConfig} from '../app/types/core/environment-config';

setupDeckGoConfig({
  app: {
    mock: true,
    cloud: 'offline'
  },
  deckdeckgo: {
    appUrl: 'http://localhost:3333',
    globalAssetsUrl: 'http://localhost:3333/assets',
    pollUrl: 'http://localhost:3333/poll',
    presentationUrl: '<@PRESENTATION_URL@>',
    socketUrl: '<@SOCKET_URL@>'
  },
  cloud: {
    cdn: 'http://localhost:3335/build/index.esm.js'
  },
  google: {
    fontsUrl: 'https://fonts.googleapis.com/css?display=swap&family='
  }
});
