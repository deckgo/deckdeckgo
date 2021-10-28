import {Deck} from '../models/data/deck';
import {Slide} from '../models/data/slide';
import {Doc} from '../models/data/doc';
import {Section} from '../models/data/section';

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

export interface SyncDataDoc {
  docId: string;
  doc?: Doc;
}

export interface SyncDataSection {
  docId: string;
  sectionId: string;
  section?: Section;
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

  updateDocs: SyncDataDoc[] | undefined;
  deleteDocs: SyncDataDoc[] | undefined;
  updateSections: SyncDataSection[] | undefined;
  deleteSections: SyncDataSection[] | undefined;

  syncedAt: Date;
}

export interface SyncPendingData {
  key: string;
  queuedAt: Date;
}

export interface SyncPendingDeck extends SyncPendingData {
  deckId: string;
}

export interface SyncPendingSlide extends SyncPendingDeck {
  slideId: string;
}

export interface SyncPendingDoc extends SyncPendingData {
  docId: string;
}

export interface SyncPendingSection extends SyncPendingDoc {
  sectionId: string;
}

export interface SyncPending {
  updateDecks: SyncPendingDeck[];
  deleteDecks: SyncPendingDeck[];
  updateSlides: SyncPendingSlide[];
  deleteSlides: SyncPendingSlide[];

  updateDocs: SyncPendingDoc[];
  deleteDocs: SyncPendingDoc[];
  updateSections: SyncPendingSection[];
  deleteSections: SyncPendingSection[];
}
