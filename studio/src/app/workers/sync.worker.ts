import {get, getMany} from 'idb-keyval';

import {Slide} from '../models/data/slide';
import {Deck} from '../models/data/deck';

import {SyncData, SyncDataDeck, SyncDataSlide, SyncPending, SyncPendingDeck, SyncPendingSlide} from '../types/editor/sync-data';

// TODO: move Firestore merge to worker

let timer: NodeJS.Timeout = undefined;

export const startSyncTimer = async () => {
  timer = setInterval(async () => await syncData(), 5000);
};

export const stopSyncTimer = async () => {
  if (!timer) {
    return;
  }

  await syncData();

  clearInterval(timer);
  timer = undefined;
};

const syncData = async () => {
  // TODO: Avoid atomic errors, window updating while worker running. If we can move Firestore and ICP to the worker it solves everything though.

  const data: SyncData | undefined = await collectData();

  // Do not stress window side if there are no data to sync
  if (!data) {
    return;
  }

  // @ts-ignore
  postMessage({
    msg: 'deckdeckgo_sync',
    data
  });
};

const collectData = async (): Promise<SyncData | undefined> => {
  const data: SyncPending | undefined = await get<SyncPending>('deckdeckgo_pending_sync');

  if (!data) {
    return undefined;
  }

  const updateDecks: SyncDataDeck[] | undefined = (await getMany(uniqueSyncData(data.updateDecks).map(({key}: SyncPendingDeck) => key))).map((deck: Deck) => ({
    deckId: deck.id,
    deck
  }));

  const deleteDecks: SyncDataDeck[] | undefined = uniqueSyncData(data.deleteDecks).map(({deckId}: SyncPendingDeck) => ({deckId}));

  const updateSlides: SyncDataSlide[] | undefined = await Promise.all(uniqueSyncData(data.updateSlides).map((slide: SyncPendingSlide) => getSlide(slide)));

  const deleteSlides: SyncDataSlide[] | undefined = uniqueSyncData(data.deleteSlides).map(({deckId, slideId}: SyncPendingSlide) => ({deckId, slideId}));

  return {
    updateDecks,
    deleteDecks,
    updateSlides,
    deleteSlides
  };
};

const getSlide = async ({deckId, slideId, key}: SyncPendingSlide): Promise<SyncDataSlide> => {
  const slide: Slide | undefined = await get(key);

  return {
    deckId,
    slideId,
    slide
  };
};

const uniqueSyncData = (data: SyncPendingDeck[]): SyncPendingDeck[] => {
  return data.reduce((acc: SyncPendingDeck[], curr: SyncPendingDeck) => {
    if (acc.findIndex(({key}: SyncPendingDeck) => key === curr.key) === -1) {
      acc.push(curr);
    }

    return acc;
  }, []);
};
