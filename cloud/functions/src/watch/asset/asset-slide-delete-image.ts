import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';
import {EventContext} from 'firebase-functions';

import {SlideData} from '../../model/slide';

import {filterAssetPath, findAsset, findImages, updateAsset} from './utils/asset-utils';
import {Asset} from '../../model/asset';

export async function deleteSlideImageAsset(snap: DocumentSnapshot, context: EventContext) {
    const deckId: string = context.params.deckId;

    if (!deckId || deckId === undefined || deckId === '') {
        return;
    }

    const previousValue: SlideData = snap.data() as SlideData;

    try {
        const previousImages: HTMLElement[] | undefined = await findImages(previousValue);

        const previousAssets: string[] | undefined = await filterAssetPath(previousImages);

        if (!previousAssets || previousAssets.length <= 0) {
            return;
        }

        const asset: Asset | undefined = await findAsset(deckId);

        if (!asset ||  !asset.data || !asset.data.images || asset.data.images.length <= 0) {
            return;
        }

        previousAssets.forEach((assetPath: string) => {
            if (asset.data.images !== undefined && asset.data.images.indexOf(assetPath) > -1) {
                asset.data.images.splice(asset.data.images.indexOf(assetPath), 1);
            }
        });

        await updateAsset(deckId, asset.data);
    } catch (err) {
        console.error(err);
    }
}
