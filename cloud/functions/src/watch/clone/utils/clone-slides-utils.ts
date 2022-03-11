import * as admin from 'firebase-admin';
import {Deck} from '../../../model/data/deck';
import {Slide} from '../../../model/data/slide';
import {findDeck} from '../../../utils/data/deck-utils';
import {findSlide} from '../../../utils/data/slide-utils';

export function cloneSlides(deckIdTo: string, deckIdFrom: string): Promise<string[] | undefined> {
  return new Promise<string[] | undefined>(async (resolve, reject) => {
    try {
      // We have to iterate on the deck.data.slides because it contains the order the slides
      const deckFrom: Deck = await findDeck(deckIdFrom);

      if (!deckFrom || !deckFrom.data || !deckFrom.data.slides || deckFrom.data.slides.length <= 0) {
        resolve(undefined);
        return;
      }

      const promises: Promise<string>[] = [];
      deckFrom.data.slides.forEach((slideId: string) => {
        promises.push(cloneSlide(deckIdTo, deckIdFrom, slideId));
      });

      if (promises && promises.length > 0) {
        const newSlideIds: string[] = await Promise.all(promises);
        resolve(newSlideIds);
        return;
      }

      resolve(undefined);
    } catch (err) {
      reject(err);
    }
  });
}

export function updateCloneData(deckId: string, slidesIds?: string[] | undefined): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!deckId || deckId === undefined || deckId === '') {
        resolve();
        return;
      }

      const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/decks/${deckId}/`);

      const updateData: any = {
        clone: admin.firestore.FieldValue.delete(),
        updated_at: admin.firestore.Timestamp.now(),
      };

      if (slidesIds !== undefined && slidesIds.length > 0) {
        updateData['slides'] = slidesIds;
      }

      await documentReference.set(updateData, {merge: true});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function cloneSlide(deckIdTo: string, deckIdFrom: string, slideId: string): Promise<string> {
  return new Promise<string>(async (resolve, reject) => {
    try {
      // Find original slide
      const slide: Slide = await findSlide(deckIdFrom, slideId);

      // Clone data
      const slideData = {...slide.data};

      const now: admin.firestore.Timestamp = admin.firestore.Timestamp.now();
      slideData.created_at = now;
      slideData.updated_at = now;

      // Create cloned data
      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/decks/${deckIdTo}/slides/`);

      collectionRef.add(slideData).then(
        async (doc: admin.firestore.DocumentReference) => {
          resolve(doc.id);
        },
        (err) => {
          reject(err);
        }
      );
    } catch (err) {
      reject(err);
    }
  });
}
