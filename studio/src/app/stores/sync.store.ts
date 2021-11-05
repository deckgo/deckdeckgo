import {createStore} from '@stencil/store';

import {SyncState} from '@deckdeckgo/editor';

import {syncBeforeUnload} from '../utils/core/before-unload.utils';

interface SyncStore {
  sync: SyncState;
}

const {state, onChange} = createStore<SyncStore>({
  sync: 'idle'
});

onChange('sync', (sync: SyncState) => syncBeforeUnload(sync));

export default {state, onChange};
