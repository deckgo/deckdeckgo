import {createStore} from '@stencil/store';

import { set } from 'idb-keyval';

import { SyncState } from '../types/editor/sync';

interface SyncStore {
  sync: SyncState;
}

const {state, onChange} = createStore<SyncStore>({
  sync: 'idle'
});

onChange('sync', (sync: SyncState) => {
  set('deckdeckgo_sync_state', sync).catch((err) => {
    console.error('Failed to update IDB with sync state', err);
  });
});

export default {state};
