import {Sync, SyncData} from '@deckdeckgo/editor';
import {Identity} from '@dfinity/agent';

import {getIdentity} from '../auth/auth.ic';

import {InternetIdentityAuth} from '../../types/identity';

import {internetIdentityAuth} from '../../utils/identity.utils';
import {syncDeckBackground} from '../../utils/sync.window.utils';

import {uploadWorker} from '../../workers/sync.ic.worker';

import {SyncWindow, SyncWindowEvent} from '../../types/sync.window';

// - we cannot use postmessage because of CORS
// - we have to path the function separately for serialisation reason
const syncWindow: SyncWindow = async ({msg, data}: SyncWindowEvent) => {
  if (msg !== 'deckdeckgo_sync_deck_background') {
    return;
  }

  await syncDeckBackground(data);
};

export const sync: Sync = async ({
  syncData,
  clean
}: {
  syncData: SyncData | undefined;
  userId: string;
  clean: ({syncedAt}: SyncData) => Promise<void>;
}) => {
  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    throw new Error('No internet identity to sync data');
  }

  const internetIdentity: InternetIdentityAuth = await internetIdentityAuth();

  await uploadWorker(
    {
      internetIdentity,
      syncData,
      host: `${window.location}`
    },
    syncWindow
  );

  await clean(syncData);
};
