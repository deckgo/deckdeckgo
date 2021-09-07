import {del, get, keys, update} from 'idb-keyval';

import syncStore from '../../stores/sync.store';
import authStore from '../../stores/auth.store';

import {SyncData, SyncPending, SyncPendingDeck} from '../../types/editor/sync';

export abstract class SyncService {
  abstract upload(syncData: SyncData | undefined): Promise<void>;

  protected async clean({syncedAt}: SyncData) {
    await this.filterPending(syncedAt);

    await this.initSyncState();
  }

  protected isSyncPending = (): boolean => syncStore.state.sync === 'pending';

  private async filterPending(syncedAt: Date) {
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

  async initSyncState() {
    if (!authStore.state.loggedIn) {
      syncStore.state.sync = 'idle';
      return;
    }

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

  async clear() {
    // If the user is logged in, the data might be synced by next cron iteration. Therefore we only clean data if signed out after user has click "New deck".
    if (authStore.state.loggedIn) {
      return;
    }

    await del('deckdeckgo_pending_sync');

    const storageKeys: string[] = (await keys<string>()).filter((key: string) => key.startsWith('/decks/') || key.startsWith('/assets/'));

    if (!storageKeys.length) {
      return;
    }

    await Promise.all(storageKeys.map((key: string) => del(key)));
  }
}
