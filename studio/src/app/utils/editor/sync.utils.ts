import {update} from 'idb-keyval';

import syncStore from '../../stores/sync.store';
import authStore from '../../stores/auth.store';

import {SyncPending, SyncPendingDeck, SyncPendingSlide} from '../../types/editor/sync';

// We push data only and eliminate duplicates on the worker side when preparing the data

export const syncUpdateDeck = async (deckId: string) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncDeckSlides();
    }

    data.updateDecks.push(pendingDeck(deckId));

    return data;
  });

  setPendingState();
};

export const syncDeleteDeck = async (deckId: string) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncDeckSlides();
    }

    data.deleteDecks.push(pendingDeck(deckId));

    return data;
  });

  setPendingState();
};

export const syncUpdateSlide = async ({deckId, slideId}: {deckId: string; slideId: string}) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncDeckSlides();
    }

    data.updateSlides.push(pendingSlide({deckId, slideId}));

    return data;
  });

  setPendingState();
};

export const syncDeleteSlide = async ({deckId, slideId}: {deckId: string; slideId: string}) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncDeckSlides();
    }

    data.deleteSlides.push(pendingSlide({deckId, slideId}));

    return data;
  });

  setPendingState();
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

const setPendingState = () => {
  if (!authStore.state.loggedIn || !navigator.onLine) {
    return;
  }

  if (syncStore.state.sync === 'in_progress') {
    return;
  }

  syncStore.state.sync = 'pending';
};
