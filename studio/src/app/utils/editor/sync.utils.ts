import {update} from 'idb-keyval';

import { SyncPending } from '../../types/editor/sync-data';

// We push data only and eliminate duplicates on the worker side when preparing the data

export const syncUpdateDeck = async (deckId: string) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncDeckSlides();
    }

    data.updateDecks.push({
      deckId,
      key: `/decks/${deckId}`
    });

    return data;
  });
};

export const syncDeleteDeck = async (deckId: string) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncDeckSlides();
    }

    data.deleteDecks.push({
      deckId,
      key: `/decks/${deckId}`
    });

    return data;
  });
};

export const syncUpdateSlide = async ({deckId, slideId}: {deckId: string; slideId: string}) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncDeckSlides();
    }

    data.updateSlides.push({
      deckId,
      slideId,
      key: `/decks/${deckId}/slides/${slideId}`
    });

    return data;
  });
};

export const syncDeleteSlide = async ({deckId, slideId}: {deckId: string; slideId: string}) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncDeckSlides();
    }

    data.deleteSlides.push({
      deckId,
      slideId,
      key: `/decks/${deckId}/slides/${slideId}`
    });

    return data;
  });
};

const initSyncDeckSlides = (): SyncPending => ({updateDecks: [], deleteDecks: [], deleteSlides: [], updateSlides: []});
