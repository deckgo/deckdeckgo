import {EnvironmentAppConfig} from '../../types/core/environment-config';

import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

import {SyncProvider} from './sync.provider';
import {SyncOfflineProvider} from './sync.offline.provider';
import {SyncFirebaseProvider} from './sync.firebase.provider';
import {SyncIcProvider} from './sync.ic.provider';

export class SyncFactoryProvider {
  private static instance: SyncProvider;

  static getInstance(): SyncProvider {
    if (!SyncFactoryProvider.instance) {
      const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');
      SyncFactoryProvider.instance =
        cloud === 'offline' ? new SyncOfflineProvider() : cloud === 'ic' ? new SyncIcProvider() : new SyncFirebaseProvider();
    }
    return SyncFactoryProvider.instance;
  }
}
