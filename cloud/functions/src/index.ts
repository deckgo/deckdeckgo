import * as functions from 'firebase-functions';

import * as admin from 'firebase-admin';
const app: admin.app.App = admin.initializeApp();
app.firestore().settings({timestampsInSnapshots: true});

import {applyWatchDeckWrite} from './watch/watch-deck-write';

const runtimeOpts = {
    memory: <const> '1GB'
};

export const watchDeckWrite = functions.runWith(runtimeOpts).firestore.document('decks/{deckId}').onWrite(applyWatchDeckWrite);
