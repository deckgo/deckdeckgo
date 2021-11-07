import firebase from 'firebase/app';

import {del, get, set} from 'idb-keyval';

import {Deck, DeckAttributes, deckSelector, StorageFile, SyncDataDeck} from '@deckdeckgo/editor';

import {prepareAttributes} from '../data/firestore.utils';

import {updateDeck} from '../../providers/data/deck.firebase';
import {uploadFile} from '../../providers/storage/storage.firebase';

export const uploadDecks = async ({data, userId}: {data: SyncDataDeck[] | undefined; userId: string}): Promise<void> => {
  if (!data || data.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = data.map((deck: SyncDataDeck) => uploadDeck({data: deck, userId}));
  await Promise.all(promises);
};

const uploadDeck = async ({data, userId}: {data: SyncDataDeck; userId: string}): Promise<void> => {
  const {deckId}: SyncDataDeck = data;

  await uploadDeckBackgroundAssets({deckId, userId});

  const deck: Deck = await get(`/decks/${deckId}`);

  if (!deck) {
    return;
  }

  await uploadDeckData({
    id: deckId,
    data: {
      ...deck.data,
      owner_id: userId
    }
  });
};

const uploadDeckData = (deck: Deck): Promise<Deck> => {
  return new Promise<Deck>(async (resolve, reject) => {
    try {
      if (!deck || !deck.data) {
        reject('Missing deck for upload');
        return;
      }

      deck.data.attributes = prepareAttributes<DeckAttributes>(deck.data.attributes);

      if (deck.data.background === null) {
        // @ts-ignore
        deck.data.background = firebase.firestore.FieldValue.delete();
      }

      if (deck.data.header === null) {
        // @ts-ignore
        deck.data.header = firebase.firestore.FieldValue.delete();
      }

      if (deck.data.footer === null) {
        // @ts-ignore
        deck.data.footer = firebase.firestore.FieldValue.delete();
      }

      const persistedDeck: Deck = await updateDeck(deck);

      resolve(persistedDeck);
    } catch (err) {
      reject(err);
    }
  });
};

const uploadDeckBackgroundAssets = ({deckId, userId}: {deckId: string; userId: string}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    const backgroundElement: HTMLElement = document.querySelector(`${deckSelector} > *[slot="background"]`);

    if (!backgroundElement) {
      resolve();
      return;
    }

    try {
      const img: HTMLDeckgoLazyImgElement = backgroundElement.querySelector('deckgo-lazy-img');

      if (!img) {
        resolve();
        return;
      }

      await uploadDeckLocalImage({img, deckId, userId});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

const uploadDeckLocalImage = ({img, deckId, userId}: {img: HTMLDeckgoLazyImgElement; deckId: string; userId: string}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const data: File = await get(img.imgSrc);

      if (!data) {
        // We didn't the corresponding image. Instead of crashing an error we go through, user will notice that nothing is displayed.
        // Better than blocking the all process and reaching an intermediate state.
        resolve();
        return;
      }

      // 1. We upload the file to the storage cloud
      const storageFile: StorageFile | undefined = await uploadFile({
        data,
        folder: 'images',
        maxSize: 10485760,
        userId
      });

      if (!storageFile) {
        reject(`Image ${img.imgSrc} upload has failed.`);
        return;
      }

      // 2. We update the indexedDB stored deck with the new downloadUrl. This stored deck will be later updated back to the database.
      const deck: Deck = await get(`/decks/${deckId}`);

      if (!deck) {
        reject('Deck not found and that is really weird here.');
        return;
      }

      deck.data.background = deck.data.background.replace(`img-src="${img.imgSrc}"`, `img-src="${storageFile.downloadUrl}"`);
      deck.data.background = deck.data.background.replace(`img-alt="${img.imgSrc}"`, `img-alt="${storageFile.downloadUrl}"`);

      await set(`/decks/${deck.id}`, deck);

      // 3. We update the DOM
      img.imgSrc = storageFile.downloadUrl;
      img.imgAlt = storageFile.downloadUrl;

      // 4. All good, we don't need the image in the indexedDB anymore
      await del(img.imgSrc);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};
