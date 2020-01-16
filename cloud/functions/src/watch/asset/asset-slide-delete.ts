import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';
import {EventContext} from 'firebase-functions';

import {SlideData} from '../../model/slide';

import {extractAssetPath, findAsset, findImages, updateAsset} from './utils/asset-utils';
import {Asset} from '../../model/asset';

export async function deleteSlideAsset(snap: DocumentSnapshot, context: EventContext) {
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

function filterAssetPath(images: HTMLElement[] | undefined): Promise<string[] | undefined> {
    return new Promise<string[]|undefined>(async (resolve) => {
        if (!images || images === undefined || images.length <= 0) {
            resolve(undefined);
            return;
        }

        const results: string[] = [];
        for (const image of images) {
            const path: string | null = image.getAttribute('img-src');
            const assetPath: string | undefined = await extractAssetPath(path);

            if (assetPath !== undefined) {
                results.push(assetPath);
            }
        }

        if (results.length <= 0) {
            resolve(undefined);
            return;
        }

        resolve(results);
    });
}
