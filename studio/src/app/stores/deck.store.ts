import {createStore} from '@stencil/store';

import {Deck} from '@deckdeckgo/editor';

import {setEditDeckId} from '../utils/editor/editor.utils';

interface DeckStore {
  deck: Deck | null;
  name: string | null;
  published: boolean;
}

const {state, onChange, reset} = createStore<DeckStore>({
  deck: null,
  name: null,
  published: false
});

onChange('deck', (deck: Deck | null) => {
  state.name = deck?.data?.name && deck?.data?.name !== '' ? deck.data.name : null;
  state.published = deck?.data?.meta?.published ?? false;

  if (!deck) {
    return;
  }

  setEditDeckId(deck.id).catch((err) => {
    console.error('Failed to update IDB with new deck id', err);
  });
});

export default {state, onChange, reset};
