import authStore from '../../stores/auth.store';
import offlineStore from '../../stores/offline.store';

import {EnvironmentAppConfig} from '../../types/core/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

import {StorageIcProvider} from './storage.ic.provider';
import {StorageFirebaseProvider} from './storage.firebase.provider';
import {StorageOfflineProvider} from './storage.offline.provider';

export interface StorageProvider {
  uploadFile(data: File, folder: string, maxSize: number, downloadUrl?: boolean): Promise<StorageFile | undefined>;

  getFiles(next: string | null, folder: string): Promise<StorageFilesList | null>;

  getFolders(folder: string): Promise<StorageFoldersList | undefined>;
}

export const getStorageService = (): StorageProvider => {
  if (authStore.state.loggedIn && offlineStore.state.online) {
    return getOnlineStorageService();
  }

  return StorageOfflineProvider.getInstance();
};

export const getOnlineStorageService = (): StorageProvider => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
  return cloud === 'ic' ? StorageIcProvider.getInstance() : StorageFirebaseProvider.getInstance();
};
