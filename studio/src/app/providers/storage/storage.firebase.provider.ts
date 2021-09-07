import firebase from '@firebase/app';
import '@firebase/storage';

import {Reference, ListResult, ListOptions} from '@firebase/storage-types';

import errorStore from '../../stores/error.store';
import authStore from '../../stores/auth.store';

import {Constants} from '../../types/core/constants';

import {StorageProvider} from './storage.provider';

export class StorageFirebaseProvider implements StorageProvider {
  private static instance: StorageFirebaseProvider;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!StorageFirebaseProvider.instance) {
      StorageFirebaseProvider.instance = new StorageFirebaseProvider();
    }
    return StorageFirebaseProvider.instance;
  }

  // @Override
  uploadFile(data: File, folder: string, maxSize: number, downloadUrl: boolean = true): Promise<StorageFile | undefined> {
    return new Promise<StorageFile>(async (resolve) => {
      try {
        if (!authStore.state.authUser || !authStore.state.authUser.uid || authStore.state.authUser.uid === '' || authStore.state.authUser.uid === undefined) {
          errorStore.state.error = 'Not logged in.';
          resolve(undefined);
          return;
        }

        if (!data || !data.name) {
          errorStore.state.error = 'File not valid.';
          resolve(undefined);
          return;
        }

        if (data.size > maxSize) {
          errorStore.state.error = `File is too big (max. ${maxSize / 1048576} Mb)`;
          resolve(undefined);
          return;
        }

        const ref: Reference = firebase.storage().ref(`${authStore.state.authUser.uid}/assets/${folder}/${data.name}`);

        // Firebase issue: updating a File/Blob which has been saved previously in IDB does not work.
        const buffer: ArrayBuffer = await new Response(data).arrayBuffer();

        await ref.put(buffer);

        resolve({
          downloadUrl: downloadUrl ? await ref.getDownloadURL() : undefined,
          fullPath: ref.fullPath,
          name: ref.name
        });
      } catch (err) {
        errorStore.state.error = err.message;
        resolve(undefined);
      }
    });
  }

  // @Override
  getFiles(next: string | null, folder: string): Promise<StorageFilesList | null> {
    return new Promise<StorageFilesList | null>(async (resolve) => {
      try {
        if (!authStore.state.authUser || !authStore.state.authUser.uid || authStore.state.authUser.uid === '' || authStore.state.authUser.uid === undefined) {
          errorStore.state.error = 'Not logged in.';
          resolve(null);
          return;
        }

        const ref = firebase.storage().ref(`${authStore.state.authUser.uid}/assets/${folder}`);

        let options: ListOptions = {
          maxResults: Constants.STORAGE.MAX_QUERY_RESULTS
        };

        if (next) {
          options.pageToken = next;
        }

        const results: ListResult = await ref.list(options);

        resolve(this.toStorageFilesList(results));
      } catch (err) {
        resolve(null);
      }
    });
  }

  private toStorageFilesList(results: ListResult): Promise<StorageFilesList> {
    return new Promise<StorageFilesList>(async (resolve) => {
      if (!results || !results.items || results.items.length <= 0) {
        resolve({
          items: [],
          nextPageToken: null
        });
        return;
      }

      const storageFiles: Promise<StorageFile>[] = results.items.map(this.toStorageFile);
      const items: StorageFile[] = await Promise.all(storageFiles);

      resolve({
        items: items,
        nextPageToken: results.nextPageToken
      });
    });
  }

  private toStorageFile(ref: Reference): Promise<StorageFile> {
    return new Promise<StorageFile>(async (resolve) => {
      resolve({
        downloadUrl: await ref.getDownloadURL(),
        fullPath: ref.fullPath,
        name: ref.name
      });
    });
  }

  // @Override
  getFolders(folder: string): Promise<StorageFoldersList | undefined> {
    return new Promise<StorageFoldersList | null>(async (resolve) => {
      try {
        if (!authStore.state.authUser || !authStore.state.authUser.uid || authStore.state.authUser.uid === '' || authStore.state.authUser.uid === undefined) {
          errorStore.state.error = 'Not logged in.';
          resolve(undefined);
          return;
        }

        const ref = firebase.storage().ref(`${authStore.state.authUser.uid}/assets/${folder}/`);

        const results: ListResult = await ref.listAll();

        resolve(this.toStorageFoldersList(results));
      } catch (err) {
        resolve(null);
      }
    });
  }

  private toStorageFoldersList(results: ListResult): StorageFoldersList | undefined {
    if (!results || !results.prefixes || results.prefixes.length <= 0) {
      return undefined;
    }

    return {
      prefixes: results.prefixes.map((prefix: Reference) => ({name: prefix.name}))
    };
  }
}
