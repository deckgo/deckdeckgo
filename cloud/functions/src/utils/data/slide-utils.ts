import * as admin from 'firebase-admin';

import {Slide, SlideData} from '../../model/data/slide';

export function findSlide(deckId: string, slideId: string): Promise<Slide> {
  return new Promise<Slide>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/decks/${deckId}/slides/${slideId}`).get();

      if (!snapshot.exists) {
        reject('Deck not found');
        return;
      }

      resolve({
        id: snapshot.id,
        ref: snapshot.ref,
        data: snapshot.data() as SlideData,
      });
    } catch (err) {
      reject(err);
    }
  });
}
