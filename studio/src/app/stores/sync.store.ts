import {SyncState} from '@deckdeckgo/editor';
import {syncBeforeUnload, syncSubscribe} from '@deckdeckgo/sync';
import {createStore} from '@stencil/store';

interface SyncStore {
  sync: SyncState;
  dirty: boolean;
}

const {state, onChange} = createStore<SyncStore>({
  sync: 'idle',
  dirty: false
});

syncSubscribe((syncState: SyncState) => {
  state.sync = syncState;
});

onChange('sync', (sync: SyncState) => {
  state.dirty = ['pending', 'in_progress', 'init'].includes(sync);

  syncBeforeUnload(state.dirty);
});

export default {state, onChange};
