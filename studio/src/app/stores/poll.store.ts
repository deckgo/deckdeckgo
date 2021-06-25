import {createStore} from '@stencil/store';

import {DeckdeckgoPoll} from '@deckdeckgo/types';

interface PollStore {
  poll: DeckdeckgoPoll | undefined;
}

const {state, onChange, reset} = createStore({
  poll: undefined
} as PollStore);

export default {state, onChange, reset};
