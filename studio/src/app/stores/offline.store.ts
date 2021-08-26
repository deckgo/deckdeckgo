import {createStore} from '@stencil/store';

interface OfflineStore {
  offline: OfflineDeck | undefined;
  online: boolean;
}

const {state, onChange, reset} = createStore({
  offline: undefined,
  online: navigator?.onLine || false
} as OfflineStore);

window.addEventListener(
  'online',
  () => {
    state.online = true;
  },
  {passive: true}
);

window.addEventListener(
  'offline',
  () => {
    state.online = false;
  },
  {passive: true}
);

export default {state, onChange, reset};
