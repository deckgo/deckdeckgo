import {createStore} from '@stencil/store';

interface BusyStore {
  deckBusy: boolean | undefined;
  slideEditable: HTMLElement | undefined;
}

const {state, onChange} = createStore({
  deckBusy: undefined,
  slideEditable: undefined,
} as BusyStore);

export default {state, onChange};
