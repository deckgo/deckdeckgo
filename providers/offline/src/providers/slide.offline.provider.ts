import type {Slide, SlideData} from '@deckdeckgo/editor';
import {get, set, update} from 'idb-keyval';
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
  update(`/decks/${deckId}/slides/${slide.id}`, ({data, ...rest}: Slide) => ({
    ...rest,
    data: {
      ...data,
      ...slide.data,
      updated_at: new Date()
    }
  }));

export const deleteOfflineSlide = ({deckId, slideId}: {deckId: string; slideId: string}): Promise<void> =>
  update(`/decks/${deckId}/slides/${slideId}`, ({data, ...rest}: Slide) => ({
    ...rest
  }));
