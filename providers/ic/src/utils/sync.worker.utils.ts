import {del, get} from 'idb-keyval';

import {StorageFile} from '@deckdeckgo/editor';

import {uploadFileIC} from '../providers/storage/storage.ic';

import {SyncWindow} from '../types/sync.window';
import {Identity} from '@dfinity/agent';

export const uploadDeckLocalImage = ({
  imgSrc,
  deckId,
  host,
  identity,
  syncWindow
}: {
  imgSrc: string;
  deckId: string;
  host: string;
  identity: Identity;
  syncWindow: SyncWindow;
}): Promise<StorageFile | undefined> => {
  return new Promise<StorageFile | undefined>(async (resolve, reject) => {
    try {
      const data: File = await get(imgSrc);

      if (!data) {
        // We didn't the corresponding image. Instead of crashing an error we go through, user will notice that nothing is displayed.
        // Better than blocking the all process and reaching an intermediate state.
        resolve(undefined);
        return;
      }

      // 1. We upload the file to the storage cloud
      const storageFile: StorageFile | undefined = await uploadFileIC({
        data,
        folder: 'images',
        maxSize: 10485760,
        identity,
        host
      });

      if (!storageFile) {
        reject(`Image ${imgSrc} upload has failed.`);
        return;
      }

      // 2. We update the DOM and IDB (currently saved data)
      await syncWindow({
        msg: 'deckdeckgo_sync_deck_background',
        data: {
          imgSrc,
          deckId,
          storageFile
        }
      });

      // 3. All good, we don't need the image in the indexedDB anymore
      await del(imgSrc);

      resolve(storageFile);
    } catch (err) {
      reject(err);
    }
  });
};
