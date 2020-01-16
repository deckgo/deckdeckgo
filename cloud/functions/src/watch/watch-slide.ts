import {Change, EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {updateSlideImageAsset} from './asset/asset-slide-update-image';
import {deleteSlideImageAsset} from './asset/asset-slide-delete-image';
import {updateSlideDataAsset} from './asset/asset-slide-update-data';
import {deleteSlideDataAsset} from './asset/asset-slide-delete-data';
import {createSlideDataAsset} from './asset/asset-slide-create-data';

export async function applyWatchSlideUpdate(change: Change<DocumentSnapshot>, context: EventContext) {
    await updateSlideImageAsset(change, context);
    await updateSlideDataAsset(change, context);
}

export async function applyWatchSlideDelete(snapshot: DocumentSnapshot, context: EventContext) {
    await deleteSlideImageAsset(snapshot, context);
    await deleteSlideDataAsset(snapshot, context);
}

export async function applyWatchSlideCreate(snapshot: DocumentSnapshot, context: EventContext) {
    await createSlideDataAsset(snapshot, context);
}
