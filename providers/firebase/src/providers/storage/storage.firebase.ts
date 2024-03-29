import firebase from '@firebase/app';
import '@firebase/storage';

import {Reference, ListResult, ListOptions} from '@firebase/storage-types';

import {GetFiles, StorageFile, StorageFilesList, UploadFile, DeleteFile} from '@deckdeckgo/editor';

export const uploadFile: UploadFile = ({
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

export const getFiles: GetFiles = ({
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

export const deleteFile: DeleteFile = async ({fullPath}: StorageFile): Promise<void> => {
  const ref: Reference = firebase.storage().ref(fullPath);
  return ref.delete();
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
