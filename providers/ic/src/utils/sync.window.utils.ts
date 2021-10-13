import {get, set} from 'idb-keyval';

import {Deck, deckSelector} from '@deckdeckgo/editor';

import {SyncICDeckBackground} from '../types/sync';

import {updateDeckBackground} from './img.utils';

export const uploadDeckLocalImage = async (data: SyncICDeckBackground): Promise<void> => {
  // 1. We update the DOM
  updateDOM(data);

  // 2. We update the indexedDB stored deck with the new downloadUrl.
  await updateIDB(data);
};

const updateDOM = ({storageFile}: SyncICDeckBackground) => {
  const backgroundElement: HTMLElement | null = document.querySelector(`${deckSelector} > *[slot="background"]`);

  const img: HTMLDeckgoLazyImgElement | null = backgroundElement?.querySelector('deckgo-lazy-img');

  if (!img) {
    return;
  }

  const {downloadUrl, name} = storageFile;

  img.imgSrc = downloadUrl;
  img.imgAlt = name;
};

const updateIDB = async ({storageFile, imgSrc, deckId}: SyncICDeckBackground) => {
  const deck: Deck = await get(`/decks/${deckId}`);

  if (!deck) {
    throw new Error('Deck not found and that is really weird here.');
  }

  const updateDeck: Deck = updateDeckBackground({deck, imgSrc, storageFile});

  await set(`/decks/${deck.id}`, updateDeck);
};
