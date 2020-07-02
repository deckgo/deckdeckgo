import {createStore} from '@stencil/store';

import {Deck} from '../models/data/deck';

interface FeedStore {
  decks: Deck[] | undefined;
  lastPageReached: boolean;
}

const {state, reset} = createStore({
  decks: undefined,
  lastPageReached: false,
} as FeedStore);

export default {state, reset};
