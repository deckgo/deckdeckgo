import {Deck, DeckData} from '@deckdeckgo/editor';
import {syncUpdateDeck} from '@deckdeckgo/studio';
import {get, set} from 'idb-keyval';
import {v4 as uuid} from 'uuid';

export class DeckOfflineProvider {
  private static instance: DeckOfflineProvider;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!DeckOfflineProvider.instance) {
      DeckOfflineProvider.instance = new DeckOfflineProvider();
    }
    return DeckOfflineProvider.instance;
  }

  create(deckData: DeckData): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      try {
        const deckId: string = uuid();

        const now: Date = new Date();

        const deck: Deck = {
          id: deckId,
          data: {
            ...deckData,
            updated_at: now,
            created_at: now
          }
        };

        await set(`/decks/${deckId}`, deck);

        await syncUpdateDeck(deckId);

        resolve(deck);
      } catch (err) {
        reject(err);
      }
    });
  }

  get(deckId: string): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      try {
        const deck: Deck = await get(`/decks/${deckId}`);

        resolve(deck);
      } catch (err) {
        reject(err);
      }
    });
  }

  update(deck: Deck): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      try {
        if (!deck || !deck.data) {
          reject('Invalid deck data');
          return;
        }

        deck.data.updated_at = new Date();

        await set(`/decks/${deck.id}`, deck);

        await syncUpdateDeck(deck.id);

        resolve(deck);
      } catch (err) {
        reject(err);
      }
    });
  }

  async entries(_userId: string): Promise<Deck[]> {
    throw new Error('Not implemented');
  }

  delete(_deckId: string): Promise<void> {
    throw new Error('Not implemented');
  }
}
