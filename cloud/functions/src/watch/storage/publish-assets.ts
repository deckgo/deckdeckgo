import {Change, EventContext} from 'firebase-functions';
import * as admin from 'firebase-admin';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {DeckData} from '../../model/deck';
import {Asset} from '../../model/asset';

import {isDeckPublished} from '../screenshot/utils/update-deck';
import {findAsset} from '../asset/utils/asset-utils';

export async function publishAssets(change: Change<DocumentSnapshot>, context: EventContext) {
    const newValue: DeckData = change.after.data() as DeckData;

    const previousValue: DeckData = change.before.data() as DeckData;

    if (!newValue || !newValue.meta || !newValue.meta.published || !newValue.meta.pathname) {
        return;
    }

    if (!newValue.owner_id || newValue.owner_id === undefined || newValue.owner_id === '') {
        return;
    }

    const deckId: string = context.params.deckId;

    if (!deckId || deckId === undefined || deckId === '') {
        return
    }

    const update: boolean = await isDeckPublished(previousValue, newValue);

    if (!update) {
        return;
    }

    try {
        const asset: Asset | undefined = await findAsset(deckId);

        if (!asset ||  !asset.data) {
            return;
        }

        if ((!asset.data.data || asset.data.data.length <= 0) || (!asset.data.images || asset.data.images.length <= 0)) {
            return;
        }

        await updateFilesMetadataPublic(asset.data.images);
        await updateFilesMetadataPublic(asset.data.data);
    } catch (err) {
        console.error(err);
    }
}

function updateFilesMetadataPublic(assets: string[] | undefined): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        if (!assets || assets === undefined || assets.length <= 0) {
            resolve();
            return;
        }

        const promises: Promise<void>[] | undefined = assets.map((asset: string) => {
            return updateFileMetadataPublic(asset);
        });

        if (!promises || promises === undefined || promises.length <= 0) {
            resolve();
            return;
        }

        await Promise.all(promises);

        resolve();
    });
}

function updateFileMetadataPublic(asset: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
       try {
           const bucket = admin.storage().bucket();

           const file = bucket.file(asset);

           if (!file) {
               resolve();
               return;
           }

           await updateMetadataPublic(file);

           resolve();
       } catch (err) {
           reject(err);
        }
    });
}

function updateMetadataPublic(file: any): Promise<void> {
    return new Promise<void>(async (resolve) => {
        const [metadata] = await file.getMetadata();

        const metaPublic: boolean = metadata.metadata.visible === 'true';

        if (!metaPublic) {
            await file.setMetadata({metadata: {visible: 'true'}});
        }

        resolve();
    });
}
