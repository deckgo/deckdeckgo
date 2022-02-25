import {Slide, SlideData} from '@deckdeckgo/editor';
import {del, get, set} from 'idb-keyval';
import {nanoid} from 'nanoid';

export const createOfflineSlide = (deckId: string, slideData: SlideData): Promise<Slide> => {
  return new Promise<Slide>(async (resolve, reject) => {
    try {
      const slideId: string = nanoid();

      const slide: Slide = {
        id: slideId,
        data: slideData
      };

      const now: Date = new Date();

      slide.data.created_at = now;
      slide.data.updated_at = now;

      await set(`/decks/${deckId}/slides/${slide.id}`, slide);

      resolve(slide);
    } catch (err) {
      reject(err);
    }
  });
};

export const getOfflineSlide = (deckId: string, slideId: string): Promise<Slide> => {
  return new Promise<Slide>(async (resolve, reject) => {
    try {
      const slide: Slide = await get(`/decks/${deckId}/slides/${slideId}`);

      resolve(slide);
    } catch (err) {
      reject(err);
    }
  });
};

export const updateOfflineSlide = (deckId: string, slide: Slide): Promise<Slide> => {
  return new Promise<Slide>(async (resolve, reject) => {
    try {
      if (!slide || !slide.data) {
        reject('Invalid slide data');
        return;
      }

      slide.data.updated_at = new Date();

      await set(`/decks/${deckId}/slides/${slide.id}`, slide);

      resolve(slide);
    } catch (err) {
      reject(err);
    }
  });
};

export const deleteOfflineSlide = (deckId: string, slideId: string): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      await del(`/decks/${deckId}/slides/${slideId}`);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};
