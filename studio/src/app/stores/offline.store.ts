import {createStore} from '@stencil/store';

interface OfflineStore {
  online: boolean;
}

const {state} = createStore({
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

export default {state};
