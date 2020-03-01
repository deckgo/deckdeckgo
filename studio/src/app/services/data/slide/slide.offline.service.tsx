import {firebase} from '@firebase/app';

import {get, set} from 'idb-keyval';

import {Slide} from '../../../models/data/slide';

import {OfflineUtils} from '../../../utils/editor/offline.utils';

export class SlideOfflineService {
  private static instance: SlideOfflineService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!SlideOfflineService.instance) {
      SlideOfflineService.instance = new SlideOfflineService();
    }
    return SlideOfflineService.instance;
  }

  get(deckId: string, slideId: string): Promise<Slide> {
    return new Promise<Slide>(async (resolve, reject) => {
      try {
        const slide: Slide = await get(`/decks/${deckId}/slides/${slideId}`);

        resolve(slide);
      } catch (err) {
        reject(err);
      }
    });
  }

  update(deckId: string, slide: Slide): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        if (!slide || !slide.data) {
          reject('Invalid slide data');
          return;
        }

        slide.data.attributes = await OfflineUtils.cleanAttributes(slide.data.attributes);

        const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
        slide.data.updated_at = now;

        await set(`/decks/${deckId}/slides/${slide.id}`, slide);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }
}
