import {createStore} from '@stencil/store';

interface BusyStore {
  busy: boolean | undefined;
  slideReady: boolean;
  deckReady: boolean;
}

const {state, onChange} = createStore<BusyStore>({
  busy: undefined,
  slideReady: false,
  deckReady: false
});

export default {state, onChange};
