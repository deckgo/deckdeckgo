import {Deck, DeckData} from '@deckdeckgo/editor';
import {get, set, update} from 'idb-keyval';
import {nanoid} from 'nanoid';

export const createOfflineDeck = async (deckData: DeckData): Promise<Deck> => {
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

  return deck;
};

export const getOfflineDeck = (deckId: string): Promise<Deck | undefined> => get(`/decks/${deckId}`);

export const updateOfflineDeck = (deck: Deck): Promise<void> =>
  update(`/decks/${deck.id}`, ({data, ...rest}: Deck) => ({
    ...rest,
    data: {
      ...data,
      ...deck.data,
      updated_at: new Date()
    }
  }));
