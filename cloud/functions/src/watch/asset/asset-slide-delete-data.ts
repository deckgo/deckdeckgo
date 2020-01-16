import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';
import {EventContext} from 'firebase-functions';

import {getDataPath} from './utils/asset-data-utils';
import {findAsset, updateAsset} from './utils/asset-utils';

import {SlideData} from '../../model/slide';
import {Asset} from '../../model/asset';

export async function deleteSlideDataAsset(snap: DocumentSnapshot, context: EventContext) {
    const deckId: string = context.params.deckId;

    if (!deckId || deckId === undefined || deckId === '') {
        return;
    }

    const previousValue: SlideData = snap.data() as SlideData;

    if (!previousValue || previousValue.template !== 'chart') {
        return;
    }

    try {
        const oldPath: string | undefined = await getDataPath(previousValue ? previousValue.attributes : undefined);

        if (oldPath === undefined) {
            return;
        }

        const asset: Asset | undefined = await findAsset(deckId);

        if (!asset ||  !asset.data || !asset.data.data || asset.data.data.length <= 0) {
            return;
        }

        if (asset.data.data !== undefined && asset.data.data.indexOf(oldPath) > -1) {
            asset.data.data.splice(asset.data.data.indexOf(oldPath), 1);
        }

        await updateAsset(deckId, asset.data);
    } catch (err) {
        console.error(err);
    }

}
