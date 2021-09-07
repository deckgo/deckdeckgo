import authStore from '../../stores/auth.store';
import offlineStore from '../../stores/offline.store';

import {EnvironmentAppConfig} from '../../types/core/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

import {StorageIcService} from './storage.ic.service';
import {StorageFirebaseService} from './storage.firebase.service';
import {StorageOfflineService} from './storage.offline.service';

export interface StorageService {
  uploadFile(data: File, folder: string, maxSize: number, downloadUrl?: boolean): Promise<StorageFile | undefined>;

  getFiles(next: string | null, folder: string): Promise<StorageFilesList | null>;

  getFolders(folder: string): Promise<StorageFoldersList | undefined>;
}

export const getStorageService = (): StorageService => {
  if (authStore.state.loggedIn && offlineStore.state.online) {
    return getOnlineStorageService();
  }

  return StorageOfflineService.getInstance();
};

export const getOnlineStorageService = (): StorageService => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
  return cloud === 'ic' ? StorageIcService.getInstance() : StorageFirebaseService.getInstance();
};
