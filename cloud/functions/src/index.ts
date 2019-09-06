import * as functions from 'firebase-functions';

import * as admin from 'firebase-admin';
const app: admin.app.App = admin.initializeApp();
app.firestore().settings({timestampsInSnapshots: true});

import {applyWatchDeckWrite} from './watch/watch-deck-write';
import {applyWatchUserDelete} from './watch/watch-user-delete';

const runtimeOpts = {
    timeoutSeconds: 120,
    memory: <const> '1GB'
};

export const watchDeckWrite = functions.runWith(runtimeOpts).firestore.document('decks/{deckId}').onWrite(applyWatchDeckWrite);

export const watchDeleteUser = functions.firestore.document('users/{userId}').onDelete(applyWatchUserDelete);
