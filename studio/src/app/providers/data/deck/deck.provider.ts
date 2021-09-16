import {Deck} from '@deckdeckgo/editor';

import {DeckIcProvider} from './deck.ic.provider';
import {DeckOfflineProvider} from './deck.offline.provider';

import {firebase, internetComputer} from '../../../utils/core/environment.utils';

export const decks = async (userId: string): Promise<Deck[]> => {
  if (internetComputer()) {
    return DeckIcProvider.getInstance().entries(userId);
  }

  if (firebase()) {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {deckEntries} = await import(cdn);

    return deckEntries(userId);
  }

  return DeckOfflineProvider.getInstance().entries(userId);
};

export const deleteDeck = async (deckId: string): Promise<void> => {
  if (internetComputer()) {
    return DeckIcProvider.getInstance().delete(deckId);
  }

  if (firebase()) {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {deleteDeck: deleteUserDeck} = await import(cdn);

    return deleteUserDeck(deckId);
  }

  return DeckOfflineProvider.getInstance().delete(deckId);
};
