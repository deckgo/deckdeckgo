import {createStore} from '@stencil/store';

interface BusyStore {
  deckBusy: boolean | undefined;
  slideReady: boolean;
  deckReady: boolean;
  docReady: boolean;
}

const {state} = createStore<BusyStore>({
  deckBusy: undefined,
  slideReady: false,
  deckReady: false,
  docReady: false
});

export default {state};
