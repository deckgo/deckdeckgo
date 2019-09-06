import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {deleteDecksSlides} from './delete-decks-slides';
import {deleteUserStorage} from './delete-user-storage';

export async function applyWatchUserDelete(snap: DocumentSnapshot, context: EventContext) {
    await deleteDecksSlides(snap, context);
    await deleteUserStorage(snap, context);
}
