import {del, delMany, get, keys, update} from 'idb-keyval';

import {Sync, SyncData, SyncPending, SyncPendingDeck} from '@deckdeckgo/editor';

import syncStore from '../../stores/sync.store';
import authStore from '../../stores/auth.store';
import offlineStore from '../../stores/offline.store';

import {firebase as firebaseEnabled, internetComputer} from '../../utils/core/environment.utils';

import {SyncIcProvider} from './sync.ic.provider';

export const sync = async (syncData: SyncData | undefined) => {
  try {
    if (!syncData) {
      return;
    }

    if (!authStore.state.loggedIn || !offlineStore.state.online) {
      return;
    }

    if (!isSyncPending()) {
      return;
    }

    if (!firebaseEnabled() && !internetComputer()) {
      return;
    }

    syncStore.state.sync = 'in_progress';

    if (internetComputer()) {
      await SyncIcProvider.getInstance().upload(syncData);
      return;
    }

    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {sync}: {sync: Sync} = await import(cdn);

    return sync({
      syncData,
      userId: authStore.state.authUser.uid,
      clean: cleanSync
    });
  } catch (err) {
    syncStore.state.sync = 'error';
    console.error(err);
  }
};

export const isSyncPending = (): boolean => syncStore.state.sync === 'pending';

export const cleanSync = async ({syncedAt}: SyncData) => {
  await filterPending(syncedAt);

  await initSyncState();
};

const filterPending = async (syncedAt: Date) => {
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
};

export const initSyncState = async () => {
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
};

export const clearSync = async () => {
  // If the user is logged in, the data might be synced by next cron iteration. Therefore we only clean data if signed out after user has click "New deck".
  if (authStore.state.loggedIn) {
    return;
  }

  await del('deckdeckgo_pending_sync');

  const storageKeys: string[] = (await keys<string>()).filter((key: string) => key.startsWith('/decks/') || key.startsWith('/assets/'));

  if (!storageKeys.length) {
    return;
  }

  await delMany(storageKeys);
};
