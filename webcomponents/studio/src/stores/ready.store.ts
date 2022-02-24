import {createStore} from '@stencil/store';

interface ReadyStore {
  slideReady: boolean;
  deckReady: boolean;
  docReady: boolean;
}

const {state, onChange} = createStore<ReadyStore>({
  slideReady: false,
  deckReady: false,
  docReady: false
});

export default {state, onChange};
