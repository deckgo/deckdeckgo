import * as functions from 'firebase-functions';

import * as admin from 'firebase-admin';
const app: admin.app.App = admin.initializeApp();
app.firestore().settings({timestampsInSnapshots: true});

import {applyWatchDeckDelete, applyWatchDeckUpdate} from './watch/watch-deck';
import {applyWatchUserDelete} from './watch/watch-user';

const runtimeOpts = {
    timeoutSeconds: 120,
    memory: <const> '1GB'
};

export const watchDeckUpdate = functions.runWith(runtimeOpts).firestore.document('decks/{deckId}').onUpdate(applyWatchDeckUpdate);

export const watchDeckDelete = functions.firestore.document('decks/{deckId}').onDelete(applyWatchDeckDelete);

export const watchUserDelete = functions.auth.user().onDelete(applyWatchUserDelete);
