import * as functions from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {SlideData} from '../../model/slide';
import {Asset, AssetData} from '../../model/asset';

import {
    createAsset,
    extractAssetPath, filterAssetPath,
    findAsset,
    findImages,
    updateAsset
} from './utils/asset-utils';

export async function updateSlideImageAsset(change: functions.Change<DocumentSnapshot>, context: functions.EventContext) {
    const newValue: SlideData = change.after.data() as SlideData;

    const previousValue: SlideData = change.before.data() as SlideData;

    if (!newValue || newValue === undefined) {
        return;
    }

    const deckId: string = context.params.deckId;

    if (!deckId || deckId === undefined || deckId === '') {
        return;
    }

    try {
        const previousImages: HTMLElement[] | undefined = await findImages(previousValue);
        const newImages: HTMLElement[] | undefined = await findImages(newValue);

        if (previousImages === undefined && newImages === undefined) {
            return;
        }

        const sameAssets: boolean = await sameImages(previousImages, newImages);

        if (sameAssets) {
            return;
        }

        const asset: Asset | undefined = await findAsset(deckId);

        const assetData: AssetData = (asset && asset !== undefined) ? asset.data : {};

        await removeImages(previousImages, assetData);

        await addImages(newImages, assetData);

        if (!assetData.images || assetData.images.length <= 0) {
            assetData.images = [];
        }

        if (asset && asset.id) {
            await createAsset(deckId, assetData);
        } else {
            await updateAsset(deckId, assetData);
        }
    } catch (err) {
        console.error(err);
    }
}

async function sameImages(previousImages: HTMLElement[] | undefined, newImages: HTMLElement[] | undefined): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
        const previousAssets: string[] | undefined = await filterAssetPath(previousImages);
        const newAssets: string[] | undefined = await filterAssetPath(newImages);

        if (previousAssets === undefined && newAssets === undefined) {
            resolve(true);
            return
        }

        if ((previousAssets !== undefined && newAssets === undefined) || (previousAssets === undefined && newAssets !== undefined)) {
            resolve(false);
            return
        }

        const same: boolean = previousAssets !== undefined && newAssets !== undefined &&
            previousAssets.length === newAssets.length &&
            previousAssets.sort().every((value: string, index: number) => {
                return value === newAssets.sort()[index];
            });

        resolve(same);
    });
}

async function removeImages(previousImages: HTMLElement[] | undefined, assetData: AssetData) {
    if (!previousImages || previousImages === undefined || previousImages.length <= 0) {
        return;
    }

    for (const image of previousImages) {
        const path: string | null = image.getAttribute('img-src');
        const assetPath: string | undefined = await extractAssetPath(path);

        if (assetPath !== undefined && assetData.images && assetData.images.indexOf(assetPath) > -1) {
            assetData.images.splice(assetData.images.indexOf(assetPath), 1);
        }
    }
}

async function addImages(newImages: HTMLElement[] | undefined, assetData: AssetData) {
    if (!newImages || newImages === undefined || newImages.length <= 0) {
        return;
    }

    for (const image of newImages) {
        const path: string | null = image.getAttribute('img-src');
        const assetPath: string | undefined = await extractAssetPath(path);

        if (assetPath !== undefined) {
            if (!assetData.images) {
                assetData.images = [];
            }

            assetData.images.push(assetPath);
        }
    }
}
