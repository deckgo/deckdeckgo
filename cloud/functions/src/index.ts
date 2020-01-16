import * as functions from 'firebase-functions';

import * as admin from 'firebase-admin';
const app: admin.app.App = admin.initializeApp();
app.firestore().settings({timestampsInSnapshots: true});

import {applyWatchDeckCreate, applyWatchDeckDelete, applyWatchDeckUpdate} from './watch/watch-deck';
import {applyWatchUserCreate, applyWatchUserDelete, applyWatchUserUpdate} from './watch/watch-user';
import {applyWatchSlideCreate, applyWatchSlideDelete, applyWatchSlideUpdate} from './watch/watch-slide';

const runtimeOpts = {
    timeoutSeconds: 300,
    memory: <const> '1GB'
};

export const watchDeckUpdate = functions.runWith(runtimeOpts).firestore.document('decks/{deckId}').onUpdate(applyWatchDeckUpdate);

export const watchDeckDelete = functions.firestore.document('decks/{deckId}').onDelete(applyWatchDeckDelete);

export const watchDeckCreate = functions.firestore.document('decks/{deckId}').onCreate(applyWatchDeckCreate);

export const watchUserUpdate = functions.firestore.document('users/{userId}').onUpdate(applyWatchUserUpdate);

export const watchUserDelete = functions.auth.user().onDelete(applyWatchUserDelete);

export const watchUserCreate = functions.auth.user().onCreate(applyWatchUserCreate);

export const watchSlideUpdate = functions.firestore.document('decks/{deckId}/slides/{slideId}').onUpdate(applyWatchSlideUpdate);

export const watchSlideDelete = functions.firestore.document('decks/{deckId}/slides/{slideId}').onDelete(applyWatchSlideDelete);

export const watchSlideCreate = functions.firestore.document('decks/{deckId}/slides/{slideId}').onCreate(applyWatchSlideCreate);
