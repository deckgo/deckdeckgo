import {StorageService} from './storage.service';

// TODO: implement storage for the internet computer

export class StorageIcService implements StorageService {
  private static instance: StorageIcService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!StorageIcService.instance) {
      StorageIcService.instance = new StorageIcService();
    }
    return StorageIcService.instance;
  }

  // @Override
  async uploadFile(_data: File, _folder: string, _maxSize: number): Promise<StorageFile | undefined> {
    return undefined;
  }

  // @Override
  async getFiles(_next: string | null, _folder: string): Promise<StorageFilesList | null> {
    return null;
  }

  // @Override
  async getFolders(_folder: string): Promise<StorageFoldersList | undefined> {
    return undefined;
  }
}
