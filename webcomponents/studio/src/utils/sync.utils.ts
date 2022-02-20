import {SyncPending, SyncPendingDeck, SyncPendingDoc, SyncPendingParagraph, SyncPendingSlide} from '@deckdeckgo/editor';
import {update} from 'idb-keyval';
import authStore from '../stores/auth.store';
import offlineStore from '../stores/offline.store';
import syncStore from '../stores/sync.store';

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

export const syncUpdateParagraph = async ({docId, paragraphId}: {docId: string; paragraphId: string}) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncPending();
    }

    data.updateParagraphs.push(pendingParagraph({docId, paragraphId}));

    return data;
  });

  setPendingState();
};

export const syncDeleteParagraph = async ({docId, paragraphId}: {docId: string; paragraphId: string}) => {
  await update('deckdeckgo_pending_sync', (data: SyncPending | undefined) => {
    if (!data) {
      data = initSyncPending();
    }

    data.deleteParagraphs.push(pendingParagraph({docId, paragraphId}));

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
  deleteParagraphs: [],
  updateParagraphs: []
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

const pendingParagraph = ({docId, paragraphId}: {docId: string; paragraphId: string}): SyncPendingParagraph => ({
  docId,
  paragraphId,
  key: `/docs/${docId}/paragraphs/${paragraphId}`,
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
