import * as admin from 'firebase-admin';

import {Platform, PlatformData} from '../../../model/platform';
import {PlatformDeckGitHubRepo, PlatformDeck, PlatformDeckData} from '../../../model/platform-deck';

export function findPlatform(userId: string): Promise<Platform> {
  return new Promise<Platform>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/platforms/${userId}/`).get();

      if (!snapshot.exists) {
        reject('Platform not found');
        return;
      }

      const data: PlatformData = snapshot.data() as PlatformData;

      resolve({
        id: snapshot.id,
        ref: snapshot.ref,
        data,
      });
    } catch (err) {
      reject(err);
    }
  });
}

export function findPlatformDeck(userId: string, deckId: string): Promise<PlatformDeck | undefined> {
  return new Promise<PlatformDeck | undefined>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/platforms/${userId}/decks/${deckId}`).get();

      if (!snapshot.exists) {
        resolve(undefined);
        return;
      }

      const data: PlatformDeckData = snapshot.data() as PlatformDeckData;

      resolve({
        id: snapshot.id,
        ref: snapshot.ref,
        data,
      });
    } catch (err) {
      reject(err);
    }
  });
}

export function updatePlatformDeck(userId: string, deckId: string, deckData: PlatformDeckData, repo: PlatformDeckGitHubRepo | undefined): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!deckId || deckId === undefined || deckId === '') {
        resolve();
        return;
      }

      if (!repo) {
        resolve();
        return;
      }

      const data: PlatformDeckData = {...deckData};
      data.github.repo = {...repo};

      data.updated_at = admin.firestore.Timestamp.now();

      if (!data.created_at) {
        data.created_at = admin.firestore.Timestamp.now();
      }

      const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/platforms/${userId}/decks/${deckId}`);

      await documentReference.set(data, {merge: true});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
