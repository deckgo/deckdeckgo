import '@ionic/core';

import 'deckdeckgo';
import 'deckdeckgo-inline-editor';
import 'deckdeckgo-remote';
import 'deckdeckgo-qrcode';
import 'deckdeckgo-highlight-code';

import {setupConfig} from '../app/services/core/environment/environment-config';

setupConfig({
    appUrl: 'http://deckdeckgo-studio-beta.firebaseapp.com',
    apiUrl: '<@API_URL@>',
    firebase: {
        apiKey: '<@FIREBASE_API_KEY@>',
        authDomain: '<@FIREBASE_AUTH_DOMAIN@>',
        databaseURL: '<@FIREBASE_DATABASE_URL@>',
        projectId: '<@FIREBASE_PROJECT_ID@>',
        storageBucket: '<@FIREBASE_STORAGE_BUCKET@>',
        messagingSenderId: '<@FIREBASE_MESSAGING_SENDER_ID@>'
    },
    tenor: {
        url: 'https://api.tenor.com/v1/',
        key: '<@TENOR_KEY@>'
    },
    unsplash: {
        url: 'https://api.unsplash.com/',
        key: '<@UNSPLASH_KEY@>'
    },
    signalingServerUrl: '<@SIGNALING_SERVER_URL@>',
    prismComponentsUrl: 'https://raw.githubusercontent.com/PrismJS/prism/886698d5b759ef46162a5723a2493f97c689dc94/components.json'
});

// IE9: https://stackoverflow.com/questions/5472938/does-ie9-support-console-log-and-is-it-a-real-function#answer-5473193
const log: any = Function.prototype.bind.call(console.log, console);
log.apply(console, ['%cDeckDeckGo', 'color: #3880ff;font-size:2rem;font-weight: 300;']);
log.apply(console, ['%cHey there, interested by our code? Lucky you, we are open source :)', 'color: black;font-size:1rem;font-weight: 300;']);
log.apply(console, ['%cCome say hi and contribute to our project on Github', 'color: black;font-size:1rem;font-weight: 300;']);
log.apply(console, ['%chttps://github.com/deckgo/deckdeckgo', 'font-size:1rem;font-weight: 300;']);
