import {Deck, Slide} from '@deckdeckgo/editor';

export type SyncState = 'pending' | 'in_progress' | 'error' | 'idle';

export interface SyncDataDeck {
  deckId: string;
  deck?: Deck;
}

export interface SyncDataSlide {
  deckId: string;
  slideId: string;
  slide?: Slide;
}

export interface SyncEvent {
  msg: 'deckdeckgo_sync';
  data: SyncData;
}

export interface SyncData {
  updateDecks: SyncDataDeck[] | undefined;
  deleteDecks: SyncDataDeck[] | undefined;
  updateSlides: SyncDataSlide[] | undefined;
  deleteSlides: SyncDataSlide[] | undefined;
  syncedAt: Date;
}

export interface SyncPendingDeck {
  deckId: string;
  key: string;
  queuedAt: Date;
}

export interface SyncPendingSlide extends SyncPendingDeck {
  slideId: string;
}

export interface SyncPending {
  updateDecks: SyncPendingDeck[];
  deleteDecks: SyncPendingDeck[];
  updateSlides: SyncPendingSlide[];
  deleteSlides: SyncPendingSlide[];
}
