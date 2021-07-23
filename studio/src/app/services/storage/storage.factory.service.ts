import {EnvironmentDeckDeckGoConfig} from '../../types/core/environment-config';

import {EnvironmentConfigService} from '../environment/environment-config.service';

import {StorageService} from './storage.service';
import {StorageOfflineService} from './storage.offline.service';
import {StorageIcService} from './storage.ic.service';
import {StorageFirebaseService} from './storage.firebase.service';

export class StorageFactoryService {
  private static instance: StorageService;

  static getInstance(): StorageService {
    if (!StorageFactoryService.instance) {
      const {cloud}: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
      StorageFactoryService.instance = cloud === 'none' ? new StorageOfflineService() : cloud === 'ic' ? new StorageIcService() : new StorageFirebaseService();
    }
    return StorageFactoryService.instance;
  }
}
