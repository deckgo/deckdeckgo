import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {deleteDeckSlides} from './delete/delete-deck-slides';
import {deleteDeckDeploy} from './delete/delete-deck-deploy';

import {cloneDeckSlides} from './clone/clone-deck-slides';

export async function applyWatchDeckDelete(snapshot: DocumentSnapshot, context: EventContext) {
  await deleteDeckSlides(snapshot, context);
  await deleteDeckDeploy(snapshot, context);
}

export async function applyWatchDeckCreate(snapshot: DocumentSnapshot, context: EventContext) {
  await cloneDeckSlides(snapshot, context);
}
