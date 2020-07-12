import {createStore} from '@stencil/store';

interface BusyStore {
  deckBusy: boolean | undefined;
  slidesEditable: boolean;
}

const {state} = createStore({
  deckBusy: undefined,
  slidesEditable: false,
} as BusyStore);

export default {state};
