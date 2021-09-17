import {createStore} from '@stencil/store';

import {SyncState} from '@deckdeckgo/editor';

interface SyncStore {
  sync: SyncState;
}

const {state, onChange} = createStore<SyncStore>({
  sync: 'idle'
});

const onBeforeUnload = ($event: BeforeUnloadEvent) => {
  if (window.location.pathname === '/signin') {
    // We do not want to present a warning when user sign in
    return;
  }

  $event.preventDefault();
  return ($event.returnValue = 'Are you sure you want to exit?');
};

onChange('sync', (sync: SyncState) => {
  if (['pending', 'in_progress'].includes(sync)) {
    window.addEventListener('beforeunload', onBeforeUnload, {capture: true});
  } else {
    window.removeEventListener('beforeunload', onBeforeUnload, {capture: true});
  }
});

export default {state, onChange};
