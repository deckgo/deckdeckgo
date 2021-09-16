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
