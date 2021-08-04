import {set, setMany} from 'idb-keyval';

import {Deck} from '../../models/data/deck';
import {Slide} from '../../models/data/slide';

export interface ImportData {
  id: string;
  deck: Deck;
  slides: Slide[];
}

export const importEditorData = async ({id, deck, slides}: ImportData) => {
  await setMany(slides.map((slide: Slide) => [`/decks/${id}/slides/${slide.id}`, slide]));
  await set(`/decks/${id}`, deck);

  await set('deckdeckgo_deck_id', id);
};
