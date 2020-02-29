import {firebase} from '@firebase/app';
import '@firebase/firestore';

import {Slide, SlideData} from '../../../models/data/slide';

export class SlideOnlineService {
  private static instance: SlideOnlineService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!SlideOnlineService.instance) {
      SlideOnlineService.instance = new SlideOnlineService();
    }
    return SlideOnlineService.instance;
  }

  get(deckId: string, slideId: string): Promise<Slide> {
    return new Promise<Slide>(async (resolve, reject) => {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      try {
        const snapshot: firebase.firestore.DocumentSnapshot = await firestore
          .collection(`/decks/${deckId}/slides`)
          .doc(slideId)
          .get();

        if (!snapshot.exists) {
          reject('Slide not found');
          return;
        }

        const slide: SlideData = snapshot.data() as SlideData;

        resolve({
          id: snapshot.id,
          data: slide
        });
      } catch (err) {
        reject(err);
      }
    });
  }

  update(deckId: string, slide: Slide): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
      slide.data.updated_at = now;

      try {
        await firestore
          .collection(`/decks/${deckId}/slides`)
          .doc(slide.id)
          .set(slide.data, {merge: true});

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }
}
