import offlineStore from '../../stores/offline.store';
import authStore from '../../stores/auth.store';

import {StorageOnlineService} from './storage.online.service';
import {StorageOfflineService} from './storage.offline.service';

export class StorageService {
  private static instance: StorageService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!StorageService.instance) {
      StorageService.instance = new StorageService();
    }
    return StorageService.instance;
  }

  async getFiles(next: string | null, folder: string): Promise<StorageFilesList | null> {
    if (this.online()) {
      return StorageOnlineService.getInstance().getFiles(next, folder);
    }

    return StorageOfflineService.getInstance().getFiles(next, folder);
  }

  async getFolders(folder: string): Promise<StorageFoldersList | undefined> {
    if (this.online()) {
      return StorageOnlineService.getInstance().getFolders(folder);
    }

    return undefined;
  }

  private online(): boolean {
    return authStore.state.loggedIn && offlineStore.state.online;
  }
}
