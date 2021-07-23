import {StorageService} from './storage.service';

// TODO: implement storage for the internet computer

export class StorageIcService extends StorageService {
  // @Override
  uploadFile(_data: File, _folder: string, _maxSize: number): Promise<StorageFile | undefined> {
    throw new Error('Not implemented yet.');
  }

  // @Override
  getFiles(_next: string | null, _folder: string): Promise<StorageFilesList | null> {
    throw new Error('Not implemented yet.');
  }

  // @Override
  getFolders(_folder: string): Promise<StorageFoldersList | undefined> {
    throw new Error('Not implemented yet.');
  }
}
