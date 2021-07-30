import {get, update} from 'idb-keyval';

import syncStore from '../../../stores/sync.store';

import {SyncData, SyncPending, SyncPendingDeck} from '../../../types/editor/sync';

export abstract class SyncService {
  abstract upload(syncData: SyncData | undefined): Promise<void>;

  protected async clean({syncedAt}: SyncData) {
    await this.cleanPending(syncedAt);

    await this.updateSyncState();
  }

  protected isSyncPending = (): boolean => syncStore.state.sync === 'pending';

  private async cleanPending(syncedAt: Date) {
    const data: SyncPending | undefined = await get<SyncPending>('deckdeckgo_pending_sync');

    if (!data) {
      return undefined;
    }

    const filter = (arr: SyncPendingDeck[]) => arr.filter(({queuedAt}: SyncPendingDeck) => queuedAt.getTime() > syncedAt.getTime());

    await update<SyncPending>(
      'deckdeckgo_pending_sync',
      (data: SyncPending) =>
        ({
          updateDecks: filter(data.updateDecks),
          deleteDecks: filter(data.deleteDecks),
          updateSlides: filter(data.updateSlides),
          deleteSlides: filter(data.deleteSlides)
        } as SyncPending)
    );
  }

  private async updateSyncState() {
    const data: SyncPending | undefined = await get<SyncPending>('deckdeckgo_pending_sync');

    if (!data) {
      syncStore.state.sync = 'idle';
      return;
    }

    const {updateDecks, deleteDecks, deleteSlides, updateSlides} = data;

    if (updateDecks.length === 0 && deleteDecks.length === 0 && deleteSlides.length === 0 && updateSlides.length === 0) {
      syncStore.state.sync = 'idle';
      return;
    }

    syncStore.state.sync = 'pending';
  }
}
