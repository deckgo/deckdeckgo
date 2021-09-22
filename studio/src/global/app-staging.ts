import {enableInputBlurring} from '../app/utils/ionic/ionic.input-blurring';
enableInputBlurring();

import {setupConfig as setupDeckGoConfig} from '../app/types/core/environment-config';

setupDeckGoConfig({
  app: {
    mock: true,
    cloud: 'ic'
  },
  deckdeckgo: {
    appUrl: 'https://deckdeckgo-studio-staging.web.app',
    globalAssetsUrl: 'https://deckdeckgo-studio-staging.web.app/assets',
    pollUrl: 'https://deckdeckgo-studio-staging.web.app/poll',
    apiUrl: '<@API_URL@>',
    presentationUrl: '<@PRESENTATION_URL@>',
    socketUrl: '<@SOCKET_URL@>'
  },
  tenor: {
    url: 'https://api.tenor.com/v1/',
    key: '<@TENOR_KEY@>'
  },
  unsplash: {
    url: '<@UNSPLASH_URL@>',
    cdn: 'http://localhost:5000/index.js'
  },
  google: {
    fontsUrl: 'https://fonts.googleapis.com/css?display=swap&family='
  },
  cloud: {
    cdn: 'http://localhost:3335/build/index.esm.js'
  }
});
