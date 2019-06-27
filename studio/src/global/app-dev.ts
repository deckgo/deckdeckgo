import '@ionic/core';

import {setupConfig} from '@ionic/core';
setupConfig({
    inputBlurring: false
});

import '@deckdeckgo/core';
import '@deckdeckgo/inline-editor';
import '@deckdeckgo/remote';
import '@deckdeckgo/qrcode';
import '@deckdeckgo/highlight-code';
import '@deckdeckgo/lazy-img';

import {setupConfig as setupDeckGoConfig} from '../app/services/core/environment/environment-config';

setupDeckGoConfig({
    appUrl: 'http://localhost:3333',
    apiUrl: '<@API_URL@>',
    presentationUrl: '<@PRESENTATION_URL@>',
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
        url: '<@UNSPLASH_URL@>'
    },
    signalingServerUrl: '<@SIGNALING_SERVER_URL@>',
    prismComponentsUrl: 'https://raw.githubusercontent.com/PrismJS/prism/886698d5b759ef46162a5723a2493f97c689dc94/components.json',
    gifExampleSrc: 'https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/200w_d.gif'
});
