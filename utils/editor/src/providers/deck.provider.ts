import {Deck, DeckData} from '../models/data/deck';

export interface DeckEntries {
  (userId: string): Promise<Deck[]>;
}

export interface DeleteDeck {
  (deckId: string): Promise<void>;
}

export interface CreateDeck {
  (deck: DeckData): Promise<Deck>;
}

export interface GetDeck {
  (deckId: string): Promise<Deck>;
}

export interface UpdateDeck {
  (deck: Deck): Promise<Deck>;
}

export interface SnapshotDeck {
  ({deckId, onNext, onError}: {deckId: string; onNext: (snapshot: Deck) => Promise<void>; onError?: (error: string) => void}): Promise<
    () => void | undefined
  >;
}
