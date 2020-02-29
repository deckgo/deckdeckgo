import {get} from 'idb-keyval';

import {Slide} from '../../../models/data/slide';

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
}
