import * as admin from 'firebase-admin';

import {Deck, DeckData} from '../../model/data/deck';

export function findDeck(deckId: string): Promise<Deck> {
  return new Promise<Deck>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/decks/${deckId}/`).get();

      if (!snapshot.exists) {
        reject('Deck not found');
        return;
      }

      const deckData: DeckData = snapshot.data() as DeckData;

      resolve({
        id: snapshot.id,
        ref: snapshot.ref,
        data: deckData,
      });
    } catch (err) {
      reject(err);
    }
  });
}

export function updateDeck(deckId: string, deckData: Partial<DeckData>): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!deckId || deckId === undefined || deckId === '') {
        reject('No deck ID provided to update.');
        return;
      }

      const data: Partial<DeckData> = {...deckData};
      data.updated_at = admin.firestore.Timestamp.now();

      const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/decks/${deckId}`);

      await documentReference.set(data, {merge: true});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
