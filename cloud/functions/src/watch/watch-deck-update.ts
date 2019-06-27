import {DocumentSnapshot} from '@google-cloud/firestore';

import {Change, EventContext} from 'firebase-functions';

import {generateDeckScreenshot} from './generate-deck-screenshot';

export async function applyDeckUpdate(change: Change<DocumentSnapshot>, context: EventContext) {
    await generateDeckScreenshot(change);
}
