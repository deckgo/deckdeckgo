import * as functions from 'firebase-functions';

import 'firebase-functions/lib/logger/compat';

import * as admin from 'firebase-admin';
const app: admin.app.App = admin.initializeApp();
app.firestore().settings({timestampsInSnapshots: true});

import {applyWatchDeckCreate, applyWatchDeckDelete} from './watch/watch-deck';
import {applyWatchUserCreate, applyWatchUserDelete, applyWatchUserUpdate} from './watch/watch-user';
import {applyWatchTaskCreate} from './watch/watch-task';
import {applyWatchImportDeck} from './watch/deck/deck-import';

import {publishTask} from './request/publish';
import {feedDecks} from './request/feed';

const runtimePublishOpts = {
  timeoutSeconds: 120,
  memory: <const>'1GB',
};

const runtimeStorageOpts = {
  timeoutSeconds: 300,
};

export const watchDeckDelete = functions.firestore.document('decks/{deckId}').onDelete(applyWatchDeckDelete);

export const watchDeckCreate = functions.firestore.document('decks/{deckId}').onCreate(applyWatchDeckCreate);

export const watchUserUpdate = functions.firestore.document('users/{userId}').onUpdate(applyWatchUserUpdate);

export const watchTaskCreate = functions.runWith(runtimePublishOpts).firestore.document('tasks/{taskId}').onCreate(applyWatchTaskCreate);

export const watchUserDelete = functions.auth.user().onDelete(applyWatchUserDelete);

export const watchUserCreate = functions.auth.user().onCreate(applyWatchUserCreate);

export const watchDeckImport = functions.runWith(runtimeStorageOpts).storage.bucket().object().onFinalize(applyWatchImportDeck);

export const publish = functions.https.onRequest(publishTask);

export const feed = functions.https.onRequest(feedDecks);
