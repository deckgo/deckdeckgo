import {SyncData} from '../../types/editor/sync';

import {SyncProvider} from './sync.provider';

export class SyncOfflineProvider extends SyncProvider {
  // @Override
  upload(_syncData: SyncData | undefined): Promise<void> {
    return Promise.resolve();
  }
}
