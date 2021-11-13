import {Deck, DeckEntries, DeckData, DeleteDeck} from '@deckdeckgo/editor';

import {deleteData, entries} from '../../utils/data.utils';

export const deckEntries: DeckEntries = async (_userId: string): Promise<Deck[]> => entries<Deck, DeckData>({filter: '/decks/'});

export const deleteDeck: DeleteDeck = async (deckId: string): Promise<void> => deleteData({key: `/decks/${deckId}`});
