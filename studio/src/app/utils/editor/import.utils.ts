import {set, setMany} from 'idb-keyval';

import {Deck, Slide} from '@deckdeckgo/editor';

import {syncUpdateDeck, syncUpdateSlide} from './sync.utils';

export interface ImportData {
  id: string;
  deck: Deck;
  slides: Slide[];
}

export interface ImportAsset {
  path: string;
  blob: Blob;
}

export const importEditorData = async ({id, deck, slides}: ImportData) => {
  await setMany(slides.map((slide: Slide) => [`/decks/${id}/slides/${slide.id}`, slide]));
  await set(`/decks/${id}`, deck);

  await set('deckdeckgo_deck_id', id);
};

export const importEditorAssets = (assets: ImportAsset[]): Promise<void> => {
  return setMany(assets.map(({path, blob}: ImportAsset) => [path, blob]));
};

export const importEditorSync = async ({deck, slides}: ImportData) => {
  await syncUpdateDeck(deck.id);

  for (let {id: slideId} of slides) {
    await syncUpdateSlide({deckId: deck.id, slideId});
  }
};
