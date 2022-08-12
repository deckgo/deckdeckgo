import {Slide, SlideData} from '@deckdeckgo/editor';
import {del, get, set, update} from 'idb-keyval';
import {nanoid} from 'nanoid';

export const createOfflineSlide = async (deckId: string, slideData: SlideData): Promise<Slide> => {
  const slideId: string = nanoid();

  const slide: Slide = {
    id: slideId,
    data: slideData
  };

  const now: Date = new Date();

  slide.data.created_at = now;
  slide.data.updated_at = now;

  await set(`/decks/${deckId}/slides/${slide.id}`, slide);

  return slide;
};

export const getOfflineSlide = (deckId: string, slideId: string): Promise<Slide | undefined> => get(`/decks/${deckId}/slides/${slideId}`);

export const updateOfflineSlide = (deckId: string, slide: Slide): Promise<void> =>
  update(`/decks/${deckId}/slides/${slide.id}`, ({data}: Slide) => ({
    id: slide.id,
    data: {
      ...data,
      ...slide.data
    }
  }));

export const deleteOfflineSlide = (deckId: string, slideId: string): Promise<void> => del(`/decks/${deckId}/slides/${slideId}`);
