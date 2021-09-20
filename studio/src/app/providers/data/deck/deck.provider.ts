import {Deck, DeckEntries, DeleteDeck, SnapshotDeck} from '@deckdeckgo/editor';

import {DeckIcProvider} from './deck.ic.provider';
import {DeckOfflineProvider} from './deck.offline.provider';

import {firebase, internetComputer} from '../../../utils/core/environment.utils';
import {cloudProvider} from '../../../utils/core/providers.utils';

export const decks = async (userId: string): Promise<Deck[]> => {
  if (internetComputer()) {
    return DeckIcProvider.getInstance().entries(userId);
  }

  if (firebase()) {
    const {deckEntries}: {deckEntries: DeckEntries} = await cloudProvider<{deckEntries: DeckEntries}>();

    return deckEntries(userId);
  }

  return DeckOfflineProvider.getInstance().entries(userId);
};

export const deleteDeck = async (deckId: string): Promise<void> => {
  if (internetComputer()) {
    return DeckIcProvider.getInstance().delete(deckId);
  }

  if (firebase()) {
    const {deleteDeck: deleteUserDeck}: {deleteDeck: DeleteDeck} = await cloudProvider<{deleteDeck: DeleteDeck}>();

    return deleteUserDeck(deckId);
  }

  return DeckOfflineProvider.getInstance().delete(deckId);
};

export const snapshotDeck = async ({
  deckId,
  onNext
}: {
  deckId: string;
  onNext: (snapshot: Deck) => void;
}): Promise<() => void | undefined> => {
  if (firebase()) {
    const {snapshotDeck: snapshotUserDeck}: {snapshotDeck: SnapshotDeck} = await cloudProvider<{snapshotDeck: SnapshotDeck}>();

    return snapshotUserDeck({deckId, onNext});
  }

  // TODO: publish?
  throw new Error('Not implemented');
};
