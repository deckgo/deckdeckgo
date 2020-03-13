import {keys, set} from 'idb-keyval';

import {ErrorService} from '../core/error/error.service';

export class StorageOfflineService {
  private static instance: StorageOfflineService;

  private errorService: ErrorService;

  private constructor() {
    this.errorService = ErrorService.getInstance();
  }

  static getInstance() {
    if (!StorageOfflineService.instance) {
      StorageOfflineService.instance = new StorageOfflineService();
    }
    return StorageOfflineService.instance;
  }

  uploadFile(data: File, folder: string, maxSize: number): Promise<StorageFile> {
    return new Promise<StorageFile>(async (resolve) => {
      try {
        if (!data || !data.name) {
          this.errorService.error('File not valid.');
          resolve();
          return;
        }

        if (data.size > maxSize) {
          this.errorService.error(`File is too big (max. ${maxSize / 1048576} Mb)`);
          resolve();
          return;
        }

        const key: string = `/assets/${folder}/${data.name}`;

        await set(key, data);

        resolve({
          downloadUrl: key,
          fullPath: key,
          name: data.name
        });
      } catch (err) {
        this.errorService.error('File could not be saved.');
        resolve();
      }
    });
  }

  getFiles(_next: string | null, folder: string): Promise<StorageFilesList | null> {
    return new Promise<StorageFilesList | null>(async (resolve) => {
      const storageKeys: IDBValidKey[] = await keys();

      if (!storageKeys || storageKeys.length <= 0) {
        resolve(null);
        return;
      }

      const filteredKeys: IDBValidKey[] = storageKeys.filter((key: IDBValidKey) => {
        return (key as string).indexOf(`/assets/${folder}/`) > -1;
      });

      if (!filteredKeys || filteredKeys.length <= 0) {
        resolve(null);
        return;
      }

      const items: StorageFile[] = filteredKeys.map((key: IDBValidKey) => {
        return {
          downloadUrl: key,
          fullPath: key,
          name: key
        } as StorageFile;
      });

      resolve({
        items,
        nextPageToken: undefined
      });
    });
  }
}
