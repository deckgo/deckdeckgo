import * as functions from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {JSDOM} from 'jsdom';

import {DeckData} from '../../model/deck';
import {Asset, AssetData} from '../../model/asset';

import {createAsset, extractAssetPath, findAsset, updateAsset} from './utils/asset-utils';

export async function updateDeckAsset(change: functions.Change<DocumentSnapshot>, context: functions.EventContext) {
    const newValue: DeckData = change.after.data() as DeckData;

    const previousValue: DeckData = change.before.data() as DeckData;

    if (!newValue || newValue === undefined) {
        return;
    }

    const deckId: string = context.params.deckId;

    if (!deckId || deckId === undefined || deckId === '') {
        return;
    }

    try {
        const previousImage: HTMLElement | undefined = await parseBackground(previousValue);
        const newImage: HTMLElement | undefined = await parseBackground(newValue);

        if (previousImage !== undefined || newImage !== undefined) {
            const asset: Asset | undefined = await findAsset(deckId);

            const assetData: AssetData = (asset && asset !== undefined) ? asset.data : {};

            await removeImage(previousImage, assetData);

            await addImage(newImage, assetData);

            if (asset && asset.id) {
                await createAsset(deckId, assetData);
            } else {
                await updateAsset(deckId, assetData);
            }
        }
    } catch (err) {
        console.error(err);
    }
}

function parseBackground(deckData: DeckData): Promise<HTMLElement | undefined> {
    return new Promise<HTMLElement | undefined>((resolve) => {
        if (!deckData || !deckData.background || deckData.background === undefined || deckData.background === '') {
            resolve(undefined);
            return;
        }

        const dom: JSDOM = new JSDOM(`<!DOCTYPE html>${deckData.background}`);

        const image: HTMLElement | null = dom.window.document.querySelector('deckgo-lazy-img');

        resolve(image ? image : undefined);
    });
}

async function removeImage(image: HTMLElement | undefined, assetData: AssetData) {
    if (image === undefined) {
        return;
    }

    const path: string | null = image.getAttribute('img-src');
    const assetPath: string | undefined = await extractAssetPath(path);

    if (assetPath !== undefined && assetData.images && assetData.images.indexOf(assetPath) > -1) {
        assetData.images.splice(assetData.images.indexOf(assetPath), 1);
    }
}

async function addImage(image: HTMLElement | undefined, assetData: AssetData) {
    if (image === undefined) {
        return;
    }

    const path: string | null = image.getAttribute('img-src');
    const assetPath: string | undefined = await extractAssetPath(path);

    if (assetPath !== undefined) {
        if (!assetData.images) {
            assetData.images = [];
        }

        assetData.images.push(assetPath);
    }
}
