import {update} from 'idb-keyval';

import {SyncPending, SyncPendingDeck, SyncPendingDoc, SyncPendingSection, SyncPendingSlide} from '@deckdeckgo/editor';

import syncStore from '../../stores/sync.store';
import authStore from '../../stores/auth.store';
import offlineStore from '../../stores/offline.store';

// We push data only and eliminate duplicates on the worker side when preparing the data

// TODO: there is probably a way to refactor these functions with the help of generic or the abstract interface...

export const syncUpdateDeck = async (deckId: string) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncPending();
    }

    data.updateDecks.push(pendingDeck(deckId));

    return data;
  });

  setPendingState();
};

export const syncDeleteDeck = async (deckId: string) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncPending();
    }

    data.deleteDecks.push(pendingDeck(deckId));

    return data;
  });

  setPendingState();
};

export const syncUpdateSlide = async ({deckId, slideId}: {deckId: string; slideId: string}) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncPending();
    }

    data.updateSlides.push(pendingSlide({deckId, slideId}));

    return data;
  });

  setPendingState();
};

export const syncDeleteSlide = async ({deckId, slideId}: {deckId: string; slideId: string}) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncPending();
    }

    data.deleteSlides.push(pendingSlide({deckId, slideId}));

    return data;
  });

  setPendingState();
};

export const syncUpdateDoc = async (docId: string) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncPending();
    }

    data.updateDocs.push(pendingDoc(docId));

    return data;
  });

  setPendingState();
};

export const syncDeleteDoc = async (docId: string) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncPending();
    }

    data.deleteDocs.push(pendingDoc(docId));

    return data;
  });

  setPendingState();
};

export const syncUpdateSection = async ({docId, sectionId}: {docId: string; sectionId: string}) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncPending();
    }

    data.updateSections.push(pendingSection({docId, sectionId}));

    return data;
  });

  setPendingState();
};

export const syncDeleteSection = async ({docId, sectionId}: {docId: string; sectionId: string}) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncPending();
    }

    data.deleteSections.push(pendingSection({docId, sectionId}));

    return data;
  });

  setPendingState();
};

const initSyncPending = (): SyncPending => ({
  updateDecks: [],
  deleteDecks: [],
  deleteSlides: [],
  updateSlides: [],
  updateDocs: [],
  deleteDocs: [],
  deleteSections: [],
  updateSections: []
});

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

const pendingDoc = (docId: string): SyncPendingDoc => ({
  docId,
  key: `/docs/${docId}`,
  queuedAt: new Date()
});

const pendingSection = ({docId, sectionId}: {docId: string; sectionId: string}): SyncPendingSection => ({
  docId,
  sectionId,
  key: `/docs/${docId}/sections/${sectionId}`,
  queuedAt: new Date()
});

const setPendingState = () => {
  if (!authStore.state.loggedIn || !offlineStore.state.online) {
    return;
  }

  if (syncStore.state.sync === 'in_progress') {
    return;
  }

  syncStore.state.sync = 'pending';
};
