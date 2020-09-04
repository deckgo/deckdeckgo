import * as admin from 'firebase-admin';

import {Slide} from '../../../model/data/slide';

export interface DeckSlides {
  deckId: string;
  slides: Slide[] | null;
}

export function findSlides(deckId: string): Promise<DeckSlides | null> {
  return new Promise<DeckSlides | null>(async (resolve, reject) => {
    try {
      if (!deckId || deckId === undefined || deckId === '') {
        resolve(null);
        return;
      }

      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/decks/${deckId}/slides/`);

      const snapShot: admin.firestore.QuerySnapshot = await collectionRef.get();

      if (snapShot && snapShot.docs && snapShot.docs.length > 0) {
        const slides: Slide[] = snapShot.docs.map((doc) => {
          const id = doc.id;
          const ref = doc.ref;

          return {
            id: id,
            ref: ref,
            data: doc.data(),
          } as Slide;
        });

        resolve({
          deckId: deckId,
          slides: slides,
        });
      } else {
        resolve(null);
      }
    } catch (err) {
      reject(err);
    }
  });
}

export function deleteSlides(deckId: string, slides: Slide[] | null): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!slides || slides.length <= 0) {
        resolve();
        return;
      }

      const promises: Promise<void>[] = [];
      slides.forEach((slide: Slide) => {
        promises.push(deleteSlide(deckId, slide));
      });

      if (promises && promises.length > 0) {
        await Promise.all(promises);
      }

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function deleteSlide(deckId: string, slide: Slide): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!slide || !slide.id || slide.id === undefined || slide.id === '') {
        resolve();
        return;
      }

      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/decks/${deckId}/slides/`);
      const doc: admin.firestore.DocumentReference = collectionRef.doc(slide.id);

      await doc.delete();

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
