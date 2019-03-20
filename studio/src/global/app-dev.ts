import '@ionic/core';

import 'deckdeckgo';
import 'deckdeckgo-inline-editor';

import {setupConfig} from '../app/services/environment/environment-config';

setupConfig({
    apiUrl: '<@API_URL@>',
    firebase: {
        apiKey: '<@FIREBASE_API_KEY@>',
        authDomain: '<@FIREBASE_AUTH_DOMAIN@>',
        databaseURL: '<@FIREBASE_AUTH_DOMAIN@>',
        projectId: '<@FIREBASE_PROJECT_ID@>',
        storageBucket: '<@FIREBASE_STORAGE_BUCKET@>',
        messagingSenderId: '<@FIREBASE_MESSAGING_SENDER_ID@>'
    }
});
