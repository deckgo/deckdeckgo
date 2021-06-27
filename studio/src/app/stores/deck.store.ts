import {createStore} from '@stencil/store';

import { set } from 'idb-keyval';

import {Deck} from '../models/data/deck';

interface DeckStore {
  deck: Deck | null;
  name: string | null;
  published: boolean;
}

const {state, onChange, reset} = createStore({
  deck: null,
  name: null,
  published: false
} as DeckStore);

onChange('deck', (deck: Deck | null) => {
  state.name = deck?.data?.name && deck?.data?.name !== '' ? deck.data.name : null;
  state.published = deck?.data?.meta?.published ?? false;

  if (!deck) {
    return;
  }

  set('deckdeckgo_deck_id', deck.id).catch((err) => {
    console.error('Failed to update IDB with new deck id', err);
  });
});

export default {state, onChange, reset};
