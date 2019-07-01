import {Change, EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {generateDeckScreenshot} from './generate-deck-screenshot';

export async function applyWatchDeckWrite(change: Change<DocumentSnapshot>, context: EventContext) {
    await generateDeckScreenshot(change);
}
