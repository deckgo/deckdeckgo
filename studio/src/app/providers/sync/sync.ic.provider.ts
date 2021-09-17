import {Identity} from '@dfinity/agent';

import {SyncData} from '@deckdeckgo/editor';

import authStore from '../../stores/auth.store';
import syncStore from '../../stores/sync.store';
import offlineStore from '../../stores/offline.store';

import {InternetIdentityAuth} from '../../types/core/ic.identity';

import {internetComputer} from '../../utils/core/environment.utils';
import {internetIdentityAuth} from '../../utils/core/ic.identity.utils';

import {AuthIcProvider} from '../auth/auth.ic.provider';
import {cleanSync, isSyncPending} from './sync.provider';

import {uploadWorker} from '../../workers/sync.ic.worker';

export class SyncIcProvider {
  private static instance: SyncIcProvider;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!SyncIcProvider.instance) {
      SyncIcProvider.instance = new SyncIcProvider();
    }
    return SyncIcProvider.instance;
  }

  async upload(syncData: SyncData | undefined) {
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

      if (!internetComputer()) {
        return;
      }

      const identity: Identity | undefined = AuthIcProvider.getInstance().getIdentity();

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

      await cleanSync(syncData);
    } catch (err) {
      syncStore.state.sync = 'error';
      console.error(err);
    }
  }
}
