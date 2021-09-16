import {SyncData} from '@deckdeckgo/editor';

import {SyncProvider} from './sync.provider';

export class SyncOfflineProvider extends SyncProvider {
  // @Override
  upload(_syncData: SyncData | undefined): Promise<void> {
    return Promise.resolve();
  }
}
