import {log, Sync, SyncData} from '@deckdeckgo/editor';
import {Identity} from '@dfinity/agent';
import {InternetIdentityAuth} from '../../types/identity';
import {SyncWindow, SyncWindowEvent} from '../../types/sync.window';
import {internetIdentityAuth} from '../../utils/identity.utils';
import {syncDeckBackground, syncParagraphImage, syncSlideChart, syncSlideImage} from '../../utils/sync.window.utils';
import {uploadWorker} from '../../workers/sync.ic.worker';
import {getIdentity} from '../auth/auth.ic';

// - we cannot use postmessage because of CORS
// - we have to path the function separately in the function's call for serialisation reason (not within the object)
const syncWindow: SyncWindow = async ({msg, data}: SyncWindowEvent) => {
  if (msg === 'deckdeckgo_sync_deck_background') {
    await syncDeckBackground(data);
  }

  if (msg === 'deckdeckgo_sync_slide_image') {
    await syncSlideImage(data);
  }

  if (msg === 'deckdeckgo_sync_slide_chart') {
    await syncSlideChart(data);
  }

  if (msg === 'deckdeckgo_sync_paragraph_image') {
    await syncParagraphImage(data);
  }
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
      syncData
    },
    syncWindow,
    log
  );

  await clean(syncData);
};
