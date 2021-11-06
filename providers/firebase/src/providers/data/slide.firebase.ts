import {DeleteSlide, Slide, SlideData, UpdateSlide} from '@deckdeckgo/editor';
import {deleteEntry, getEntry, updateEntry} from '../../utils/firestore.queries';

export const getSlide = (deckId: string, slideId: string): Promise<Slide> => {
  return getEntry<SlideData>({id: slideId, collection: `/decks/${deckId}/slides`});
};

export const updateSlide: UpdateSlide = (deckId: string, slide: Slide): Promise<Slide> => {
  return updateEntry<Slide>({entry: slide, collection: `/decks/${deckId}/slides`});
};

export const deleteSlide: DeleteSlide = (deckId: string, slideId: string): Promise<void> => {
  return deleteEntry({id: slideId, collection: `/decks/${deckId}/slides`});
};
