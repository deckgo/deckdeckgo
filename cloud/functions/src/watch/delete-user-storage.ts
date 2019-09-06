import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import * as functions from 'firebase-functions';
import * as admin from 'firebase-admin';

export async function deleteUserStorage(snap: DocumentSnapshot, context: functions.EventContext) {
    const userId: string = context.params.userId;

    if (!userId || userId === undefined || userId === '') {
        return;
    }

    try {
        await deleteStorage(userId);
    } catch (err) {
        console.error(err);
    }
}

function deleteStorage(userId: string | null): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        try {
            if (!userId || userId === undefined || userId === '') {
                resolve();
                return;
            }

            const bucket = admin.storage().bucket();
            await bucket.deleteFiles({prefix: `${userId}/`});

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}
