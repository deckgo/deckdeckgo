import {SyncData} from '../../../types/editor/sync';

import {SyncService} from './sync.service';

export class SyncOfflineService extends SyncService {
  // @Override
  upload(_syncData: SyncData | undefined): Promise<void> {
    return Promise.resolve();
  }
}
