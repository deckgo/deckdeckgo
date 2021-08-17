import {createStore} from '@stencil/store';

import {set} from 'idb-keyval';

import {SyncState} from '../types/editor/sync';

interface SyncStore {
  sync: SyncState;
}

const {state, onChange} = createStore<SyncStore>({
  sync: 'idle'
});

const onBeforeUnload = ($event: BeforeUnloadEvent) => {
  $event.preventDefault();
  return ($event.returnValue = 'Are you sure you want to exit?');
};

onChange('sync', (sync: SyncState) => {
  set('deckdeckgo_sync_state', sync).catch((err) => {
    console.error('Failed to update IDB with sync state', err);
  });

  if (['pending', 'in_progress'].includes(sync)) {
    window.addEventListener('beforeunload', onBeforeUnload, {capture: true});
  } else {
    window.removeEventListener('beforeunload', onBeforeUnload, {capture: true});
  }
});

export default {state, onChange};
