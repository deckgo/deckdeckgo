import {createStore} from '@stencil/store';

interface BusyStore {
  deckBusy: boolean | undefined;
  slideReady: boolean;
}

const {state} = createStore({
  deckBusy: undefined,
  slideReady: false
} as BusyStore);

export default {state};
