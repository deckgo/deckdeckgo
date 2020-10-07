import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {DeckData} from '../../model/data/deck';

import {deleteDeployForId} from './utils/delete-platform-utils';

export async function deleteDeckDeploy(snap: DocumentSnapshot, context: EventContext) {
  const deckId: string = context.params.deckId;

  if (!deckId || deckId === undefined || deckId === '') {
    return;
  }

  const deck: DeckData = snap.data() as DeckData;

  if (!deck || deck === undefined) {
    return;
  }

  try {
    await deleteDeployForId(deckId);
  } catch (err) {
    console.error(err);
  }
}
