import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {deleteDecksSlides} from './delete-decks-slides';

export async function applyWatchUserDelete(snap: DocumentSnapshot, context: EventContext) {
    await deleteDecksSlides(snap, context);
    // TODO: delete storage
}
