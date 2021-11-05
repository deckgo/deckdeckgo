import {createStore} from '@stencil/store';

interface BusyStore {
  busy: boolean | undefined;
  slideReady: boolean;
  deckReady: boolean;
  docReady: boolean;
}

const {state} = createStore<BusyStore>({
  busy: undefined,
  slideReady: false,
  deckReady: false,
  docReady: false
});

export default {state};
