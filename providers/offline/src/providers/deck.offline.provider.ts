import {Deck, DeckData} from '@deckdeckgo/editor';
import {get, set} from 'idb-keyval';
import {nanoid} from 'nanoid';

export const createOfflineDeck = (deckData: DeckData): Promise<Deck> => {
  return new Promise<Deck>(async (resolve, reject) => {
    try {
      const deckId: string = nanoid();

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

      resolve(deck);
    } catch (err) {
      reject(err);
    }
  });
};

export const getOfflineDeck = (deckId: string): Promise<Deck> => {
  return new Promise<Deck>(async (resolve, reject) => {
    try {
      const deck: Deck = await get(`/decks/${deckId}`);

      resolve(deck);
    } catch (err) {
      reject(err);
    }
  });
};

export const updateOfflineDeck = (deck: Deck): Promise<Deck> => {
  return new Promise<Deck>(async (resolve, reject) => {
    try {
      if (!deck || !deck.data) {
        reject('Invalid deck data');
        return;
      }

      deck.data.updated_at = new Date();

      await set(`/decks/${deck.id}`, deck);

      resolve(deck);
    } catch (err) {
      reject(err);
    }
  });
};
