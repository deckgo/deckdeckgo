import * as admin from 'firebase-admin';

import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {DeckData} from '../../model/deck';

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
    const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/platforms/${deck.owner_id}/decks/`);
    const doc: admin.firestore.DocumentReference = collectionRef.doc(deckId);

    await doc.delete();
  } catch (err) {
    console.error(err);
  }
}
