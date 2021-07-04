import {update} from 'idb-keyval';

import { SyncPending, SyncPendingDeck, SyncPendingSlide } from '../../types/editor/sync-data';

// We push data only and eliminate duplicates on the worker side when preparing the data

export const syncUpdateDeck = async (deckId: string) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncDeckSlides();
    }

    data.updateDecks.push(pendingDeck(deckId));

    return data;
  });
};

export const syncDeleteDeck = async (deckId: string) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncDeckSlides();
    }

    data.deleteDecks.push(pendingDeck(deckId));

    return data;
  });
};

export const syncUpdateSlide = async ({deckId, slideId}: {deckId: string; slideId: string}) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncDeckSlides();
    }

    data.updateSlides.push(pendingSlide({deckId, slideId}));

    return data;
  });
};

export const syncDeleteSlide = async ({deckId, slideId}: {deckId: string; slideId: string}) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncDeckSlides();
    }

    data.deleteSlides.push(pendingSlide({deckId, slideId}));

    return data;
  });
};

const initSyncDeckSlides = (): SyncPending => ({updateDecks: [], deleteDecks: [], deleteSlides: [], updateSlides: []});

const pendingDeck = (deckId: string): SyncPendingDeck => ({
  deckId,
  key: `/decks/${deckId}`,
  queuedAt: new Date()
});

const pendingSlide = ({deckId, slideId}: {deckId: string; slideId: string}): SyncPendingSlide => ({
  deckId,
  slideId,
  key: `/decks/${deckId}/slides/${slideId}`,
  queuedAt: new Date()
});
