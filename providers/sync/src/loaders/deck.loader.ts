import {Deck} from '@deckdeckgo/editor';
import {getOfflineDeck} from '@deckdeckgo/offline';
import {DeckStore} from '../stores/deck.store';

export const loadDeck = async (deckId: string | undefined): Promise<Deck> => {
  if (!deckId) {
    DeckStore.getInstance().set(null);
    throw new Error('Deck is not defined');
  }

  const deck: Deck = await getOfflineDeck(deckId);

  if (!deck || !deck.data) {
    DeckStore.getInstance().set(null);
    throw new Error('No deck could be fetched');
  }

  DeckStore.getInstance().set({...deck});

  return deck;
};

export const resetDeck = () => DeckStore.getInstance().set(null);
