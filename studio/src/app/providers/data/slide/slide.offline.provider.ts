import {Slide, SlideData} from '@deckdeckgo/editor';
import {syncDeleteSlide, syncUpdateSlide} from '@deckdeckgo/studio';
import {del, get, set} from 'idb-keyval';
import {v4 as uuid} from 'uuid';

export class SlideOfflineProvider {
  private static instance: SlideOfflineProvider;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!SlideOfflineProvider.instance) {
      SlideOfflineProvider.instance = new SlideOfflineProvider();
    }
    return SlideOfflineProvider.instance;
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

        slide.data.created_at = now;
        slide.data.updated_at = now;

        await set(`/decks/${deckId}/slides/${slide.id}`, slide);

        await syncUpdateSlide({deckId, slideId: slide.id});

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

  update(deckId: string, slide: Slide): Promise<Slide> {
    return new Promise<Slide>(async (resolve, reject) => {
      try {
        if (!slide || !slide.data) {
          reject('Invalid slide data');
          return;
        }

        slide.data.updated_at = new Date();

        await set(`/decks/${deckId}/slides/${slide.id}`, slide);

        await syncUpdateSlide({deckId, slideId: slide.id});

        resolve(slide);
      } catch (err) {
        reject(err);
      }
    });
  }

  delete(deckId: string, slideId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        await del(`/decks/${deckId}/slides/${slideId}`);

        await syncDeleteSlide({deckId, slideId});

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }
}
