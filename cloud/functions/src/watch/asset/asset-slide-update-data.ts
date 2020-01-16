import * as functions from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {SlideData} from '../../model/slide';
import {Asset, AssetData} from '../../model/asset';

import {createAsset, findAsset, updateAsset} from './utils/asset-utils';
import {getDataPath} from './utils/asset-data-utils';

export async function updateSlideDataAsset(change: functions.Change<DocumentSnapshot>, context: functions.EventContext) {
    const newValue: SlideData = change.after.data() as SlideData;

    const previousValue: SlideData = change.before.data() as SlideData;

    if (!newValue || newValue === undefined) {
        return;
    }

    if (newValue.template !== 'chart') {
        return;
    }

    const deckId: string = context.params.deckId;

    if (!deckId || deckId === undefined || deckId === '') {
        return;
    }

    try {
        const newPath: string | undefined = await getDataPath(newValue.attributes);
        const oldPath: string | undefined = await getDataPath(previousValue ? previousValue.attributes : undefined);

        if (newPath === undefined && oldPath === undefined) {
            return;
        }

        if (newPath === oldPath) {
            return;
        }

        const asset: Asset | undefined = await findAsset(deckId);

        const assetData: AssetData = (asset && asset !== undefined) ? asset.data : {};

        await removeData(oldPath, assetData);

        await addData(newPath, assetData);

        if (asset && asset.id) {
            await createAsset(deckId, assetData);
        } else {
            await updateAsset(deckId, assetData);
        }

    } catch (err) {
        console.error(err);
    }
}

async function removeData(assetPath: string | undefined, assetData: AssetData) {
    if (assetData === undefined) {
        return;
    }

    if (assetPath !== undefined && assetData.data && assetData.data.indexOf(assetPath) > -1) {
        assetData.data.splice(assetData.data.indexOf(assetPath), 1);
    }
}

async function addData(assetPath: string | undefined, assetData: AssetData) {
    if (assetPath === undefined) {
        return;
    }

    if (!assetData.data) {
        assetData.data = [];
    }

    assetData.data.push(assetPath);
}
