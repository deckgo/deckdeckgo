import {Deck, DeckEntries, DeleteDeck, SnapshotDeck} from '@deckdeckgo/editor';
import {updateOfflineDeck} from '@deckdeckgo/offline';
import {DeckStore} from '../stores/deck.store';
import {EnvStore} from '../stores/env.store';
import {cloudProvider} from '../utils/providers.utils';

export const decks = async (userId: string): Promise<Deck[]> => {
  if (EnvStore.getInstance().cloud()) {
    const {deckEntries}: {deckEntries: DeckEntries} = await cloudProvider<{deckEntries: DeckEntries}>();

    return deckEntries(userId);
  }

  throw new Error('Not implemented');
};

export const deleteDeck = async (deckId: string): Promise<void> => {
  if (EnvStore.getInstance().cloud()) {
    const {deleteDeck: deleteUserDeck}: {deleteDeck: DeleteDeck} = await cloudProvider<{deleteDeck: DeleteDeck}>();

    return deleteUserDeck(deckId);
  }

  throw new Error('Not implemented');
};

export const snapshotDeck = (): Promise<() => void | undefined> =>
  snapshotDeckUser({
    deckId: DeckStore.getInstance().get().id,
    onNext: async (snapshot: Deck) => {
      const deck: Deck = {...snapshot};

      // Update store and replicate data to local because it has been updated in the publish process
      DeckStore.getInstance().set(deck);
      await updateOfflineDeck(deck);
    }
  });

const snapshotDeckUser = async ({deckId, onNext}: {deckId: string; onNext: (snapshot: Deck) => Promise<void>}): Promise<() => void | undefined> => {
  if (EnvStore.getInstance().cloud()) {
    const {snapshotDeck: snapshotUserDeck}: {snapshotDeck: SnapshotDeck} = await cloudProvider<{snapshotDeck: SnapshotDeck}>();

    return snapshotUserDeck({deckId, onNext});
  }

  throw new Error('No publish offline');
};
