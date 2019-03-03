import '@ionic/core';

import 'deckdeckgo';

import {setupConfig} from '../app/services/environment/environment-config';

setupConfig({
    signalingServerUrl: 'https://api.deckdeckgo.com'
});

// import { setupConfig } from '@ionic/core';

// setupConfig({
//   mode: 'ios'
// });
