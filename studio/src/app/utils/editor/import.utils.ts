import {set, setMany} from 'idb-keyval';

import {Deck} from '@deckdeckgo/editor';
import {Slide} from '@deckdeckgo/editor';

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
