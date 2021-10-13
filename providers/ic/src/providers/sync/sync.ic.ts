import {Sync, SyncData} from '@deckdeckgo/editor';
import {Identity} from '@dfinity/agent';

import {getIdentity} from '../auth/auth.ic';

import {InternetIdentityAuth} from '../../types/identity';
import {SyncICEvent} from '../../types/sync';

import {internetIdentityAuth} from '../../utils/identity.utils';
import {uploadDeckLocalImage} from '../../utils/sync.window.utils';

import {worker} from '../../workers/sync.ic.worker.ts?worker';
import {uploadWorker} from '../../workers/sync.ic.worker';

worker.onmessage = async ({data}: MessageEvent<SyncICEvent>) => {
  if (!data || data.msg !== 'deckdeckgo_sync_deck_background') {
    return;
  }

  await uploadDeckLocalImage(data.data);
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

  await uploadWorker({
    internetIdentity,
    syncData,
    host: `${window.location}`
  });

  await clean(syncData);
};
