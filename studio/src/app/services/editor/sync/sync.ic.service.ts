import {Identity} from '@dfinity/agent';

import authStore from '../../../stores/auth.store';
import syncStore from '../../../stores/sync.store';

import {SyncData} from '../../../types/editor/sync';
import {InternetIdentityAuth} from '../../../types/core/ic.identity';

import {internetComputer} from '../../../utils/core/environment.utils';
import {internetIdentityAuth} from '../../../utils/core/ic.identity.utils';

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

      const internetIdentity: InternetIdentityAuth = await internetIdentityAuth();

      await uploadWorker({
        internetIdentity,
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
