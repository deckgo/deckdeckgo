import {StorageProvider} from './storage.provider';

// TODO: implement storage for the internet computer

export class StorageIcProvider implements StorageProvider {
  private static instance: StorageIcProvider;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!StorageIcProvider.instance) {
      StorageIcProvider.instance = new StorageIcProvider();
    }
    return StorageIcProvider.instance;
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
