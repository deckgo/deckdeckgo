import firebase from '@firebase/app';
import '@firebase/storage';

import {Reference, ListResult, ListOptions} from '@firebase/storage-types';

import errorStore from '../../stores/error.store';
import authStore from '../../stores/auth.store';

import {Resources} from '../../utils/core/resources';

export class StorageOnlineService {
  private static instance: StorageOnlineService;

  static getInstance() {
    if (!StorageOnlineService.instance) {
      StorageOnlineService.instance = new StorageOnlineService();
    }
    return StorageOnlineService.instance;
  }

  uploadFile(data: File, folder: string, maxSize: number): Promise<StorageFile> {
    return new Promise<StorageFile>(async (resolve) => {
      try {
        if (!authStore.state.authUser || !authStore.state.authUser.uid || authStore.state.authUser.uid === '' || authStore.state.authUser.uid === undefined) {
          errorStore.state.error = 'Not logged in.';
          resolve();
          return;
        }

        if (!data || !data.name) {
          errorStore.state.error = 'File not valid.';
          resolve();
          return;
        }

        if (data.size > maxSize) {
          errorStore.state.error = `File is too big (max. ${maxSize / 1048576} Mb)`;
          resolve();
          return;
        }

        const ref: Reference = firebase.storage().ref(`${authStore.state.authUser.uid}/assets/${folder}/${data.name}`);

        await ref.put(data);

        resolve({
          downloadUrl: await ref.getDownloadURL(),
          fullPath: ref.fullPath,
          name: ref.name,
        });
      } catch (err) {
        errorStore.state.error = err.message;
        resolve();
      }
    });
  }

  getFiles(next: string | null, folder: string): Promise<StorageFilesList | null> {
    return new Promise<StorageFilesList | null>(async (resolve) => {
      try {
        if (!authStore.state.authUser || !authStore.state.authUser.uid || authStore.state.authUser.uid === '' || authStore.state.authUser.uid === undefined) {
          errorStore.state.error = 'Not logged in.';
          resolve(null);
          return;
        }

        const ref = firebase.storage().ref(`${authStore.state.authUser.uid}/assets/${folder}/`);

        let options: ListOptions = {
          maxResults: Resources.Constants.STORAGE.MAX_QUERY_RESULTS,
        };

        if (next) {
          options.pageToken = next;
        }

        const results: ListResult = await ref.list(options);

        resolve(this.toStorageFileList(results));
      } catch (err) {
        resolve(null);
      }
    });
  }

  private toStorageFileList(results: ListResult): Promise<StorageFilesList> {
    return new Promise<StorageFilesList>(async (resolve) => {
      if (!results || !results.items || results.items.length <= 0) {
        resolve({
          items: [],
          nextPageToken: null,
        });
        return;
      }

      const storageFiles: Promise<StorageFile>[] = results.items.map(this.toStorageFile);
      const items: StorageFile[] = await Promise.all(storageFiles);

      resolve({
        items: items,
        nextPageToken: results.nextPageToken,
      });
    });
  }

  private toStorageFile(ref: Reference): Promise<StorageFile> {
    return new Promise<StorageFile>(async (resolve) => {
      resolve({
        downloadUrl: await ref.getDownloadURL(),
        fullPath: ref.fullPath,
        name: ref.name,
      });
    });
  }
}
