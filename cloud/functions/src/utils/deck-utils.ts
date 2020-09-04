import * as admin from 'firebase-admin';

import {Deck, DeckData} from '../model/deck';

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
