import {createStore} from '@stencil/store';

interface SyncStore {
  sync: 'in_progress' | 'error' | 'idle';
}

const {state} = createStore<SyncStore>({
  sync: 'idle'
});

export default {state};
