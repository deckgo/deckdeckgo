import {firebase} from '@firebase/app';
import '@firebase/firestore';

import {get} from 'idb-keyval';

import {Slide, SlideData} from '../../../models/data/slide';

import {SlideOfflineService} from './slide.offline.service';
import {SlideOnlineService} from './slide.online.service';

export class SlideService {
  private static instance: SlideService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!SlideService.instance) {
      SlideService.instance = new SlideService();
    }
    return SlideService.instance;
  }

  create(deckId: string, slide: SlideData): Promise<Slide> {
    return new Promise<Slide>(async (resolve, reject) => {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
      slide.created_at = now;
      slide.updated_at = now;

      firestore
        .collection(`/decks/${deckId}/slides`)
        .add(slide)
        .then(
          async (doc: firebase.firestore.DocumentReference) => {
            resolve({
              id: doc.id,
              data: slide
            });
          },
          (err) => {
            reject(err);
          }
        );
    });
  }

  async update(deckId: string, slide: Slide): Promise<void> {
    const offline: boolean = await get('deckdeckgo_offline');

    if (offline) {
      return SlideOfflineService.getInstance().update(deckId, slide);
    } else {
      return SlideOnlineService.getInstance().update(deckId, slide);
    }
  }

  delete(deckId: string, slideId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const firestore: firebase.firestore.Firestore = firebase.firestore();

        await firestore
          .collection(`/decks/${deckId}/slides`)
          .doc(slideId)
          .delete();

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  async get(deckId: string, slideId: string): Promise<Slide> {
    const offline: boolean = await get('deckdeckgo_offline');

    if (offline) {
      return SlideOfflineService.getInstance().get(deckId, slideId);
    } else {
      return SlideOnlineService.getInstance().get(deckId, slideId);
    }
  }
}
