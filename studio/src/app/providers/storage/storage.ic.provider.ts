import {StorageFile, StorageFilesList, StorageFoldersList} from '@deckdeckgo/editor';

// TODO: implement storage for the internet computer

export class StorageIcProvider {
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

  async uploadFile(_data: File, _folder: string, _maxSize: number): Promise<StorageFile | undefined> {
    return undefined;
  }

  async getFiles(_next: string | null, _folder: string): Promise<StorageFilesList | null> {
    return null;
  }

  async getFolders(_folder: string): Promise<StorageFoldersList | undefined> {
    return undefined;
  }
}
