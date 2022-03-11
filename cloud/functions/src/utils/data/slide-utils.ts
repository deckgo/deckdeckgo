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

export function createSlide(deckId: string, slideData: SlideData): Promise<Slide> {
  return new Promise<Slide>(async (resolve, reject) => {
    try {
      const now: admin.firestore.Timestamp = admin.firestore.Timestamp.now();

      const data: SlideData = {
        ...slideData,
        created_at: now,
        updated_at: now,
      };

      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/decks/${deckId}/slides/`);
      const doc: admin.firestore.DocumentReference = await collectionRef.add(data);

      resolve({
        id: doc.id,
        ref: doc,
        data: data,
      });
    } catch (err) {
      reject(err);
    }
  });
}
