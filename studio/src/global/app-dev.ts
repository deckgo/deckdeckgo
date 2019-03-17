import '@ionic/core';

import 'deckdeckgo';
import 'deckdeckgo-inline-editor';

import {setupConfig} from '../app/services/environment/environment-config';

setupConfig({
    apiUrl: 'https://api.deckdeckgo.com'
});
