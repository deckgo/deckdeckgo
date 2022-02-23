import {SyncState} from '@deckdeckgo/editor';
import {createStore} from '@stencil/store';
import {syncBeforeUnload} from '../utils/core/before-unload.utils';

interface SyncStore {
  sync: SyncState;
  dirty: boolean;
}

const {state, onChange} = createStore<SyncStore>({
  sync: 'idle',
  dirty: false
});

onChange('sync', (sync: SyncState) => {
  state.dirty = ['pending', 'in_progress', 'init'].includes(sync);

  syncBeforeUnload(state.dirty);
});

export default {state, onChange};
