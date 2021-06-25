import {v4 as uuid} from 'uuid';

import {del, get, set} from 'idb-keyval';

import {Slide, SlideAttributes, SlideData} from '../../../models/data/slide';

import {OfflineUtils} from '../../../utils/editor/offline.utils';
import {FirestoreUtils} from '../../../utils/editor/firestore.utils';

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

        const now: Date = new Date();

        // @ts-ignore
        slide.data.created_at = now;
        // @ts-ignore
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

        slide.data.attributes = (await OfflineUtils.cleanAttributes(slide.data.attributes)) as SlideAttributes;

        if (slide.data.content && FirestoreUtils.shouldAttributeBeCleaned(slide.data.content)) {
          slide.data.content = null;
        }

        // @ts-ignore
        slide.data.updated_at = new Date();

        await set(`/decks/${deckId}/slides/${slide.id}`, slide);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  delete(deckId: string, slideId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        await del(`/decks/${deckId}/slides/${slideId}`);

        await this.saveSlidesToDelete(slideId);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private async saveSlidesToDelete(slideId: string) {
    let slidesToDelete: string[] = await get('deckdeckgo_slides_delete');

    if (!slidesToDelete) {
      slidesToDelete = [];
    }

    slidesToDelete.push(slideId);

    await set('deckdeckgo_slides_delete', slidesToDelete);
  }
}
