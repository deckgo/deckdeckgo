import {Change, EventContext} from 'firebase-functions';
import * as admin from 'firebase-admin';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {DeckData} from '../../model/deck';

import {isDeckPublished} from '../screenshot/utils/update-deck';

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
        await updateFolderMetadataPublic(newValue.owner_id, deckId, 'images');
        await updateFolderMetadataPublic(newValue.owner_id, deckId, 'data');
    } catch (err) {
        console.error(err);
    }
}

function updateFolderMetadataPublic(ownerId: string, deckId: string, folder: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
       try {
           const bucket = admin.storage().bucket();

           const options = {prefix: `${ownerId}/assets/${folder}/${deckId}/`};

           const [files] = await bucket.getFiles(options);

           if (!files || files.length <= 0) {
               resolve();
               return;
           }

           const promises = [];
           for (const file of files) {
               promises.push(updateMetadataPublic(file));
           }

           if (promises.length <= 0) {
               resolve();
               return;
           }

           await Promise.all(promises);

           resolve();
       } catch (err) {
           reject(err);
        }
    });
}

function updateMetadataPublic(file: any): Promise<void> {
    return new Promise<void>(async (resolve) => {
        const [metadata] = await file.getMetadata();

        const metaPublic: boolean = metadata.metadata.public === 'true';

        if (!metaPublic) {
            await file.setMetadata({metadata: {public: 'true'}});
        }

        resolve();
    });
}
