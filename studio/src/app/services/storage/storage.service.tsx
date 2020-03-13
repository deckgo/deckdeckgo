import {OfflineService} from '../editor/offline/offline.service';

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

  async uploadFile(data: File, folder: string, maxSize: number): Promise<StorageFile> {
    const offline: OfflineDeck = await OfflineService.getInstance().status();

    if (offline !== undefined) {
      return StorageOfflineService.getInstance().uploadFile(data, folder, maxSize);
    } else {
      return StorageOnlineService.getInstance().uploadFile(data, folder, maxSize);
    }
  }

  async getFiles(next: string | null, folder: string): Promise<StorageFilesList | null> {
    const offline: OfflineDeck = await OfflineService.getInstance().status();

    if (offline !== undefined) {
      return StorageOfflineService.getInstance().getFiles(next, folder);
    } else {
      return StorageOnlineService.getInstance().getFiles(next, folder);
    }
  }
}
