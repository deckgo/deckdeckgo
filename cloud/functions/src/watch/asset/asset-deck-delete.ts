import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';
import {EventContext} from 'firebase-functions';

import {deleteAsset} from './utils/asset-utils';

export async function deleteDeckAsset(_snap: DocumentSnapshot, context: EventContext) {
    const deckId: string = context.params.deckId;

    if (!deckId || deckId === undefined || deckId === '') {
        return;
    }

    try {
        await deleteAsset(deckId);
    } catch (err) {
        console.error(err);
    }
}
