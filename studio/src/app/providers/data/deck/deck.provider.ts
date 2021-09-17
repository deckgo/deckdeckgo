import {Deck, DeckEntries, DeleteDeck} from '@deckdeckgo/editor';

import {DeckIcProvider} from './deck.ic.provider';
import {DeckOfflineProvider} from './deck.offline.provider';

import {firebase, internetComputer} from '../../../utils/core/environment.utils';
import {provider} from '../../../utils/core/providers.utils';

export const decks = async (userId: string): Promise<Deck[]> => {
  if (internetComputer()) {
    return DeckIcProvider.getInstance().entries(userId);
  }

  if (firebase()) {
    const {deckEntries}: {deckEntries: DeckEntries} = await provider<{deckEntries: DeckEntries}>();

    return deckEntries(userId);
  }

  return DeckOfflineProvider.getInstance().entries(userId);
};

export const deleteDeck = async (deckId: string): Promise<void> => {
  if (internetComputer()) {
    return DeckIcProvider.getInstance().delete(deckId);
  }

  if (firebase()) {
    const {deleteDeck: deleteUserDeck}: {deleteDeck: DeleteDeck} = await provider<{deleteDeck: DeleteDeck}>();

    return deleteUserDeck(deckId);
  }

  return DeckOfflineProvider.getInstance().delete(deckId);
};
