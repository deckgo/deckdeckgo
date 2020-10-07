import * as functions from 'firebase-functions';

import 'firebase-functions/lib/logger/compat';

import * as admin from 'firebase-admin';
const app: admin.app.App = admin.initializeApp();
app.firestore().settings({timestampsInSnapshots: true});

import {applyWatchDeckCreate, applyWatchDeckDelete} from './watch/watch-deck';
import {applyWatchUserCreate, applyWatchUserDelete, applyWatchUserUpdate} from './watch/watch-user';
import {applyWatchTaskCreate} from './watch/watch-task';

import {publishTask} from './request/publish';

const runtimeOpts = {
  timeoutSeconds: 120,
  memory: <const>'1GB',
};

export const watchDeckDelete = functions.firestore.document('decks/{deckId}').onDelete(applyWatchDeckDelete);

export const watchDeckCreate = functions.firestore.document('decks/{deckId}').onCreate(applyWatchDeckCreate);

export const watchUserUpdate = functions.firestore.document('users/{userId}').onUpdate(applyWatchUserUpdate);

export const watchTaskCreate = functions.runWith(runtimeOpts).firestore.document('tasks/{taskId}').onCreate(applyWatchTaskCreate);

export const watchUserDelete = functions.auth.user().onDelete(applyWatchUserDelete);

export const watchUserCreate = functions.auth.user().onCreate(applyWatchUserCreate);

export const publish = functions.https.onRequest(publishTask);
