import {Change, EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {generateDeckScreenshot} from './screenshot/generate-deck-screenshot';
import {publishAssets} from './storage/publish-assets';
import {infoDeckPublish} from './info/info-deck-publish';

import {deleteDeckSlides} from './delete/delete-deck-slides';

import {cloneDeckSlides} from './clone/clone-deck-slides';

export async function applyWatchDeckUpdate(change: Change<DocumentSnapshot>, context: EventContext) {
    await publishAssets(change, context);
    await generateDeckScreenshot(change);
    await infoDeckPublish(change);
}

export async function applyWatchDeckDelete(snapshot: DocumentSnapshot, context: EventContext) {
    await deleteDeckSlides(snapshot, context);
}

export async function applyWatchDeckCreate(snapshot: DocumentSnapshot, context: EventContext) {
    await cloneDeckSlides(snapshot, context);
}
