import {createStore} from '@stencil/store';

interface BusyStore {
  deckBusy: boolean | undefined;
  slideReady: boolean;
  deckReady: boolean;
}

const {state} = createStore({
  deckBusy: undefined,
  slideReady: false,
  deckReady: false
} as BusyStore);

export default {state};
