import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {DeckData} from '../../model/deck';

import {deletePlatformDeckForIds} from './utils/delete-platform-utils';

export async function deletePlatformDeck(snap: DocumentSnapshot, context: EventContext) {
  const deckId: string = context.params.deckId;

  if (!deckId || deckId === undefined || deckId === '') {
    return;
  }

  const deck: DeckData = snap.data() as DeckData;

  if (!deck || deck === undefined) {
    return;
  }

  try {
    await deletePlatformDeckForIds(deck.owner_id, deckId);
  } catch (err) {
    console.error(err);
  }
}
