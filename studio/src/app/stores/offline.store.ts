import {createStore} from '@stencil/store';

interface OfflineStore {
  offline: OfflineDeck | undefined;
  progress: number | undefined;
}

const {state, onChange, reset} = createStore({
  offline: undefined,
  progress: undefined
} as OfflineStore);

export default {state, onChange, reset};
