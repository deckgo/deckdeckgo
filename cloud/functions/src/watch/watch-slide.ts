import {Change, EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {updateSlideAsset} from './asset/asset-slide-update';
import {deleteSlideAsset} from './asset/asset-slide-delete';

export async function applyWatchSlideUpdate(change: Change<DocumentSnapshot>, context: EventContext) {
    await updateSlideAsset(change, context)
}

export async function applyWatchSlideDelete(snapshot: DocumentSnapshot, context: EventContext) {
    await deleteSlideAsset(snapshot, context);
}
