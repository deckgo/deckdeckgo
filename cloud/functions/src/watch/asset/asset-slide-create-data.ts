import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';
import {EventContext} from 'firebase-functions';

import {getDataPath} from './utils/asset-data-utils';
import {createAsset, findAsset, updateAsset} from './utils/asset-utils';

import {SlideData} from '../../model/slide';
import {Asset, AssetData} from '../../model/asset';

export async function createSlideDataAsset(snap: DocumentSnapshot, context: EventContext) {
    const deckId: string = context.params.deckId;

    if (!deckId || deckId === undefined || deckId === '') {
        return;
    }

    const newValue: SlideData = snap.data() as SlideData;

    if (!newValue || newValue.template !== 'chart') {
        return;
    }

    try {
        const newPath: string | undefined = await getDataPath(newValue ? newValue.attributes : undefined);

        if (newPath === undefined) {
            return;
        }

        const asset: Asset | undefined = await findAsset(deckId);

        const assetData: AssetData = (asset && asset !== undefined) ? asset.data : {};

        if (!assetData.data) {
            assetData.data = [];
        }

        assetData.data.push(newPath);

        if (asset && asset.id) {
            await createAsset(deckId, assetData);
        } else {
            await updateAsset(deckId, assetData);
        }
    } catch (err) {
        console.error(err);
    }

}
