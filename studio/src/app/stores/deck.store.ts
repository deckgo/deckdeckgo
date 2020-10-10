import {createStore} from '@stencil/store';

import {Deck} from '../models/data/deck';

interface DeckStore {
  deck: Deck | null;
  name: string | null;
  published: boolean;
}

const {state, onChange, reset} = createStore({
  deck: null,
  name: null,
  published: false,
} as DeckStore);

onChange('deck', (deck: Deck | null) => {
  state.name = deck?.data?.name && deck?.data?.name !== '' ? deck.data.name : null;
  state.published = deck?.data?.meta?.published ?? false;
});

export default {state, onChange, reset};
