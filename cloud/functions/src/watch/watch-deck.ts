import {Change, EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {generateDeckScreenshot} from './screenshot/generate-deck-screenshot';
import {infoDeckPublish} from './info/info-deck-publish';

import {deleteDeckSlides} from './delete/delete-deck-slides';
import {deletePlatformDeck} from './delete/delete-platform-deck';

import {cloneDeckSlides} from './clone/clone-deck-slides';

import {publishToGitHub} from './github/publish-github';

export async function applyWatchDeckUpdate(change: Change<DocumentSnapshot>, context: EventContext) {
  await publishToGitHub(change, context);
  await generateDeckScreenshot(change);
  await infoDeckPublish(change);
}

export async function applyWatchDeckDelete(snapshot: DocumentSnapshot, context: EventContext) {
  await deleteDeckSlides(snapshot, context);
  await deletePlatformDeck(snapshot, context);
}

export async function applyWatchDeckCreate(snapshot: DocumentSnapshot, context: EventContext) {
  await cloneDeckSlides(snapshot, context);
}
