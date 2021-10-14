import {get, set} from 'idb-keyval';

import {Deck, deckSelector} from '@deckdeckgo/editor';

import {SyncWindowDeckBackground} from '../types/sync.window';

import {updateDeckStorageData} from './sync.utils';

export const syncDeckBackground = async (data: SyncWindowDeckBackground): Promise<void> => {
  // 1. We update the deck in the DOM
  updateDeckDOM(data);

  // 2. We replicate the same changes to the slides in the DOM
  await updateSlidesDOM(data);

  // 3. We update the indexedDB stored deck with the new downloadUrl.
  await updateIDB(data);
};

const updateDeckDOM = ({storageFile}: SyncWindowDeckBackground) => {
  const backgroundElement: HTMLElement | null = document.querySelector(`${deckSelector} > *[slot="background"]`);

  const img: HTMLDeckgoLazyImgElement | null = backgroundElement?.querySelector('deckgo-lazy-img');

  if (!img) {
    return;
  }

  const {downloadUrl, name} = storageFile;

  img.imgSrc = downloadUrl;
  img.imgAlt = name;
};

const updateSlidesDOM = async ({storageFile}: SyncWindowDeckBackground) => {
  const images: NodeListOf<HTMLDeckgoLazyImgElement> = document.querySelectorAll(
    `${deckSelector} .deckgo-slide-container:not([custom-background]) *[slot="background"] deckgo-lazy-img`
  );

  if (!images) {
    return;
  }

  const updateImage = async (img: HTMLDeckgoLazyImgElement) => {
    const {downloadUrl, name} = storageFile;

    img.imgSrc = downloadUrl;
    img.imgAlt = name;
  };

  const promises: Promise<void>[] = Array.from(images).map((img: HTMLDeckgoLazyImgElement) => updateImage(img));

  if (!promises) {
    return;
  }

  await Promise.all(promises);
};

const updateIDB = async ({storageFile, src, deckId}: SyncWindowDeckBackground) => {
  const deck: Deck = await get(`/decks/${deckId}`);

  if (!deck) {
    throw new Error('Deck not found and that is really weird here.');
  }

  const updateDeck: Deck = updateDeckStorageData({deck, imgSrc: src, storageFile});

  await set(`/decks/${deck.id}`, updateDeck);
};
