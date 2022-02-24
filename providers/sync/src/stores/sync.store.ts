import {SyncState} from '@deckdeckgo/editor';
import {Store} from './store';

export class SyncStore extends Store<SyncState> {
  private static instance: SyncStore;

  private syncState: SyncState = 'idle';

  private constructor() {
    super();
  }

  static getInstance() {
    if (!SyncStore.instance) {
      SyncStore.instance = new SyncStore();
    }
    return SyncStore.instance;
  }

  set(syncState: SyncState) {
    this.syncState = syncState;

    this.populate(syncState);
  }

  get(): SyncState {
    return this.syncState;
  }

  override subscribe(callback: (data: SyncState) => void): () => void {
    const unsubscribe: () => void = super.subscribe(callback);

    callback(this.syncState);

    return unsubscribe;
  }
}
