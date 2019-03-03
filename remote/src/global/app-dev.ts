import '@ionic/core';

import 'deckdeckgo';

import {setupConfig} from '../app/services/environment/environment-config';

setupConfig({
    signalingServerUrl: location.protocol + '//' + location.hostname + ':3002'
});

// import { setupConfig } from '@ionic/core';

// setupConfig({
//   mode: 'ios'
// });
