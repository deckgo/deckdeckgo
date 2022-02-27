import {Deck, DeckData, DeckEntries, DeleteDeck, SnapshotDeck} from '@deckdeckgo/editor';
import {deleteData, entries} from '../../utils/data.utils';

export const deckEntries: DeckEntries = async (_userId: string): Promise<Deck[]> =>
  entries<Deck, DeckData>({startsWith: '/decks/', notContains: '/slides/'});

export const deleteDeck: DeleteDeck = async (deckId: string): Promise<void> => deleteData({key: `/decks/${deckId}`});

// Backwards compatibility with current publish mask in studio and Firebase support. In case of IC we actually do not need a snapshot, publish is synchronous on the client side.
export const snapshotDeck: SnapshotDeck = async ({
  onNext
}: {
  deckId: string;
  onNext: (snapshot: Deck) => void;
  onError?: (error: string) => void;
}): Promise<() => void | undefined> => {
  document.addEventListener('deckPublished', ({detail}: CustomEvent<Deck>) => onNext(detail), {passive: true});

  return () => document.removeEventListener('deckPublished', ({detail}: CustomEvent<Deck>) => onNext(detail), false);
};
