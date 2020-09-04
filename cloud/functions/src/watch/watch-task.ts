import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {publish} from './publish/publish';

export async function applyWatchTaskCreate(snapshot: DocumentSnapshot, context: EventContext) {
  await publish(snapshot, context);
}
