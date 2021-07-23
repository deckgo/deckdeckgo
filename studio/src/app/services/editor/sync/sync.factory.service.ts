import {EnvironmentDeckDeckGoConfig} from '../../../types/core/environment-config';

import {EnvironmentConfigService} from '../../environment/environment-config.service';

import {SyncService} from './sync.service';
import {SyncOfflineService} from './sync.offline.service';
import {SyncFirebaseService} from './sync.firebase.service';
import {SyncIcService} from './sync.ic.service';

export class SyncFactoryService {
  private static instance: SyncService;

  static getInstance(): SyncService {
    if (!SyncFactoryService.instance) {
      const {cloud}: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
      SyncFactoryService.instance = cloud === 'offline' ? new SyncOfflineService() : cloud === 'ic' ? new SyncIcService() : new SyncFirebaseService();
    }
    return SyncFactoryService.instance;
  }
}
