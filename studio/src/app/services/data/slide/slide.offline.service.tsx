import {firebase} from '@firebase/app';

import uuid from 'uuid/v4';

import {get, set} from 'idb-keyval';

import {Slide, SlideData} from '../../../models/data/slide';

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

  create(deckId: string, slideData: SlideData): Promise<Slide> {
    return new Promise<Slide>(async (resolve, reject) => {
      try {
        const slideId: string = uuid();

        const slide: Slide = {
          id: slideId,
          data: slideData
        };

        const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
        slide.data.created_at = now;
        slide.data.updated_at = now;

        await set(`/decks/${deckId}/slides/${slide.id}`, slide);

        resolve(slide);
      } catch (err) {
        reject(err);
      }
    });
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

        if (slide.data.content && OfflineUtils.shouldAttributeBeCleaned(slide.data.content)) {
          slide.data.content = null;
        }

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
