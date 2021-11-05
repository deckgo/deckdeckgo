import {Deck} from '../models/data/deck';
import {Slide} from '../models/data/slide';
import {Doc} from '../models/data/doc';
import {Paragraph} from '../models/data/paragraph';

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

export interface SyncDataParagraph {
  docId: string;
  paragraphId: string;
  paragraph?: Paragraph;
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
  updateParagraphs: SyncDataParagraph[] | undefined;
  deleteParagraphs: SyncDataParagraph[] | undefined;

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

export interface SyncPendingParagraph extends SyncPendingDoc {
  paragraphId: string;
}

export interface SyncPending {
  updateDecks: SyncPendingDeck[];
  deleteDecks: SyncPendingDeck[];
  updateSlides: SyncPendingSlide[];
  deleteSlides: SyncPendingSlide[];

  updateDocs: SyncPendingDoc[];
  deleteDocs: SyncPendingDoc[];
  updateParagraphs: SyncPendingParagraph[];
  deleteParagraphs: SyncPendingParagraph[];
}
