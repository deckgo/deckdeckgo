import {Change, EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {generateDeckScreenshot} from './screenshot/generate-deck-screenshot';
import {deleteDeckSlides} from './delete/delete-deck-slides';

export async function applyWatchDeckUpdate(change: Change<DocumentSnapshot>, context: EventContext) {
    await generateDeckScreenshot(change);
}

export async function applyWatchDeckDelete(snapshot: DocumentSnapshot, context: EventContext) {
    await deleteDeckSlides(snapshot, context);
}
