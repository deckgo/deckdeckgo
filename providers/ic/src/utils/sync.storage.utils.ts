import {Identity} from '@dfinity/agent';

import {del, get} from 'idb-keyval';

import {Deck, StorageFile} from '@deckdeckgo/editor';

import {SyncWindow} from '../types/sync.window';

import {uploadFileIC} from '../providers/storage/storage.ic';

export const uploadDeckBackgroundAssets = async ({
  deck,
  identity,
  host,
  syncWindow
}: {
  deck: Deck;
  identity: Identity;
  host: string;
  syncWindow: SyncWindow;
}): Promise<{imgSrc: string | undefined; storageFile: StorageFile | undefined}> => {
  const {background} = deck.data;

  if (!background) {
    return {
      imgSrc: undefined,
      storageFile: undefined
    };
  }

  const regex: RegExp = /((<deckgo-lazy-img.*?)(img-src=)(.*?")(.*?[^"]*)(.*?"))/g;

  const results: string[][] = [...background.matchAll(regex)];

  // Only one image in the background is currently supported
  if (results?.length !== 1) {
    return {
      imgSrc: undefined,
      storageFile: undefined
    };
  }

  const imgSrc: string = results[0][5];

  const storageFile: StorageFile | undefined = await uploadDeckLocalImage({imgSrc, deckId: deck.id, identity, host, syncWindow});

  return {
    imgSrc,
    storageFile
  };
};

const uploadDeckLocalImage = ({
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
