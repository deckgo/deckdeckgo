import {Identity} from '@dfinity/agent';
import {LocalStorage} from '@dfinity/auth-client';

import authStore from '../../../stores/auth.store';
import syncStore from '../../../stores/sync.store';

import {SyncData} from '../../../types/editor/sync';

import {internetComputer} from '../../../utils/core/environment.utils';

import {SyncService} from './sync.service';
import {AuthFactoryService} from '../../auth/auth.factory.service';
import {AuthIcService} from '../../auth/auth.ic.service';

import {uploadWorker} from '../../../workers/sync.ic.worker';

export class SyncIcService extends SyncService {
  // @Override
  async upload(syncData: SyncData | undefined) {
    try {
      if (!syncData) {
        return;
      }

      if (!authStore.state.loggedIn || !navigator.onLine) {
        return;
      }

      if (!this.isSyncPending()) {
        return;
      }

      if (!internetComputer()) {
        return;
      }

      const identity: Identity | undefined = (AuthFactoryService.getInstance() as AuthIcService).getIdentity();

      if (!identity) {
        return;
      }

      syncStore.state.sync = 'in_progress';

      const storage: LocalStorage = new LocalStorage('ic-');

      const identityKey: string | null = await storage.get('identity');
      const delegationChain: string | null = await storage.get('delegation');

      await uploadWorker({
        identityKey,
        delegationChain,
        syncData,
        host: `${window.location}`
      });

      await this.clean(syncData);
    } catch (err) {
      syncStore.state.sync = 'error';
      console.error(err);
    }
  }
}
