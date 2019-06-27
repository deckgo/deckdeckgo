import {Change, EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {generateDeckScreenshot} from './generate-deck-screenshot';

export async function applyDeckUpdate(change: Change<DocumentSnapshot>, context: EventContext) {
    await generateDeckScreenshot(change);
}
