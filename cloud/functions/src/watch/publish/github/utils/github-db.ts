import * as admin from 'firebase-admin';

import {Token, TokenData} from '../../../../model/data/token';
import {DeckData, DeckGitHub, DeckGitHubRepo} from '../../../../model/data/deck';

export function findToken(userId: string): Promise<Token> {
  return new Promise<Token>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/tokens/${userId}/`).get();

      if (!snapshot.exists) {
        reject('Platform not found');
        return;
      }

      const data: TokenData = snapshot.data() as TokenData;

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

export function updateDeckGitHub(deckId: string, repo: DeckGitHubRepo | undefined): Promise<void> {
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

      const data: Partial<DeckData> = {
        updated_at: admin.firestore.Timestamp.now(),
        github: {
          repo: {...repo},
        } as DeckGitHub,
      };

      const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/decks/${deckId}`);

      await documentReference.set(data, {merge: true});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
