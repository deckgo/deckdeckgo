import firebase from '@firebase/app';
import '@firebase/storage';

import {Reference, ListResult, ListOptions} from '@firebase/storage-types';

import {StorageFile, StorageFilesList, StorageFoldersList} from '@deckdeckgo/editor';

export const uploadFile = ({
  data,
  folder,
  maxSize,
  userId,
  downloadUrl = true
}: {
  data: File;
  folder: string;
  maxSize: number;
  userId: string;
  downloadUrl?: boolean;
}): Promise<StorageFile | undefined> => {
  return new Promise<StorageFile>(async (resolve, reject) => {
    try {
      if (!userId || userId === '' || userId === undefined) {
        reject('Not logged in.');
        return;
      }

      if (!data || !data.name) {
        reject('File not valid.');
        return;
      }

      if (data.size > maxSize) {
        reject(`File is too big (max. ${maxSize / 1048576} Mb)`);
        return;
      }

      const ref: Reference = firebase.storage().ref(`${userId}/assets/${folder}/${data.name}`);

      // Firebase issue: updating a File/Blob which has been saved previously in IDB does not work.
      const buffer: ArrayBuffer = await new Response(data).arrayBuffer();

      await ref.put(buffer);

      resolve({
        downloadUrl: downloadUrl ? await ref.getDownloadURL() : undefined,
        fullPath: ref.fullPath,
        name: ref.name
      });
    } catch (err) {
      reject(err.message());
    }
  });
};

export const getFiles = ({
  next,
  maxResults,
  folder,
  userId
}: {
  next: string | null;
  maxResults: number;
  folder: string;
  userId: string;
}): Promise<StorageFilesList | null> => {
  return new Promise<StorageFilesList | null>(async (resolve, reject) => {
    try {
      if (!userId || userId === '' || userId === undefined) {
        reject('Not logged in.');
        return;
      }

      const ref = firebase.storage().ref(`${userId}/assets/${folder}`);

      let options: ListOptions = {
        maxResults: maxResults
      };

      if (next) {
        options.pageToken = next;
      }

      const results: ListResult = await ref.list(options);

      resolve(toStorageFilesList(results));
    } catch (err) {
      reject(err);
    }
  });
};

const toStorageFilesList = (results: ListResult): Promise<StorageFilesList> => {
  return new Promise<StorageFilesList>(async (resolve) => {
    if (!results || !results.items || results.items.length <= 0) {
      resolve({
        items: [],
        nextPageToken: null
      });
      return;
    }

    const storageFiles: Promise<StorageFile>[] = results.items.map(toStorageFile);
    const items: StorageFile[] = await Promise.all(storageFiles);

    resolve({
      items: items,
      nextPageToken: results.nextPageToken
    });
  });
};

const toStorageFile = (ref: Reference): Promise<StorageFile> => {
  return new Promise<StorageFile>(async (resolve) => {
    resolve({
      downloadUrl: await ref.getDownloadURL(),
      fullPath: ref.fullPath,
      name: ref.name
    });
  });
};

export const getFolders = ({folder, userId}: {folder: string; userId: string}): Promise<StorageFoldersList | undefined> => {
  return new Promise<StorageFoldersList | null>(async (resolve, reject) => {
    try {
      if (!userId || userId === '' || userId === undefined) {
        reject('Not logged in.');
        return;
      }

      const ref = firebase.storage().ref(`${userId}/assets/${folder}/`);

      const results: ListResult = await ref.listAll();

      resolve(toStorageFoldersList(results));
    } catch (err) {
      reject(err);
    }
  });
};

const toStorageFoldersList = (results: ListResult): StorageFoldersList | undefined => {
  if (!results || !results.prefixes || results.prefixes.length <= 0) {
    return undefined;
  }

  return {
    prefixes: results.prefixes.map((prefix: Reference) => ({name: prefix.name}))
  };
};
