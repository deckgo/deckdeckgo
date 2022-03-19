import {createStore} from '@stencil/store';

interface ReadyStore {
  slideReady: boolean;
  deckReady: boolean;
  docReady: boolean;
}

const {state, onChange, reset} = createStore<ReadyStore>({
  slideReady: false,
  deckReady: false,
  docReady: false
});

export default {state, onChange, reset};
