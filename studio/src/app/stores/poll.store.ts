import {DeckdeckgoPoll} from '@deckdeckgo/types';
import {createStore} from '@stencil/store';

interface PollStore {
  poll: DeckdeckgoPoll | undefined;
}

const {state, onChange, reset} = createStore({
  poll: undefined
} as PollStore);

export default {state, onChange, reset};
