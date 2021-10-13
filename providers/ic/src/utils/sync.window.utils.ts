import {get, set} from 'idb-keyval';

import {Deck, deckSelector} from '@deckdeckgo/editor';

import {SyncWindowDeckBackground} from '../types/sync.window';

import {updateDeckBackgroundImage} from './img.utils';

export const syncDeckBackground = async (data: SyncWindowDeckBackground): Promise<void> => {
  // 1. We update the DOM
  updateDOM(data);

  // 2. We update the indexedDB stored deck with the new downloadUrl.
  await updateIDB(data);
};

const updateDOM = ({storageFile}: SyncWindowDeckBackground) => {
  const backgroundElement: HTMLElement | null = document.querySelector(`${deckSelector} > *[slot="background"]`);

  const img: HTMLDeckgoLazyImgElement | null = backgroundElement?.querySelector('deckgo-lazy-img');

  if (!img) {
    return;
  }

  const {downloadUrl, name} = storageFile;

  img.imgSrc = downloadUrl;
  img.imgAlt = name;
};

const updateIDB = async ({storageFile, imgSrc, deckId}: SyncWindowDeckBackground) => {
  const deck: Deck = await get(`/decks/${deckId}`);

  if (!deck) {
    throw new Error('Deck not found and that is really weird here.');
  }

  const updateDeck: Deck = updateDeckBackgroundImage({deck, imgSrc, storageFile});

  await set(`/decks/${deck.id}`, updateDeck);
};
