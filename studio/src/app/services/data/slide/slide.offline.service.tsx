import {get, set} from 'idb-keyval';

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

  update(deckId: string, slide: Slide): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const slideToPersist: Slide = await this.cleanSlide(slide);

        await set(`/decks/${deckId}/slides/${slide.id}`, slideToPersist);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private async cleanSlide(slide: Slide): Promise<Slide> {
    if (!slide || !slide.data || !slide.data.attributes) {
      return slide;
    }

    const keys: string[] = Object.keys(slide.data.attributes);

    if (!keys || keys.length <= 0) {
      return slide;
    }

    keys.forEach((key: string) => {
      const attr = slide.data.attributes[key];

      // Replace Firestore "to delete fields" with null values
      if (attr && attr._methodName && attr._methodName === 'FieldValue.delete') {
        slide.data.attributes[key] = null;
      }
    });

    return slide;
  }
}
