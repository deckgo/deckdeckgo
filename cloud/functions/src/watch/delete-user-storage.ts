import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import * as functions from 'firebase-functions';
import * as admin from 'firebase-admin';

import {ApiUser} from '../model/api.user';

export async function deleteUserStorage(snap: DocumentSnapshot, context: functions.EventContext) {
    const userId: string = context.params.userId;

    if (!userId || userId === undefined || userId === '') {
        return;
    }

    try {
        const username: string | null = await findUserName(userId);

        await deleteStorage(username);
    } catch (err) {
        console.error(err);
    }
}

function findUserName(userId: string): Promise<string | null> {
    return new Promise<string | null>(async (resolve, reject) => {
        try {
            const apiUrl: string = functions.config().api.url;

            const rawResponse: Response = await fetch(apiUrl + `/users/${userId}`, {
                method: 'GET',
                headers: {
                    'Accept': 'application/json',
                    'Content-Type': 'application/json'
                }
            });

            if (!rawResponse || !rawResponse.ok) {
                // 404 if not found
                resolve(null);
                return;
            }

            const persistedUser: ApiUser = await rawResponse.json();

            resolve(persistedUser ? persistedUser.username : null);
        } catch (err) {
            reject(err);
        }
    });
}

function deleteStorage(username: string | null): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        try {
            if (!username || username === undefined || username === '') {
                resolve();
                return;
            }

            const bucket = admin.storage().bucket();
            await bucket.deleteFiles({prefix: `${username}/`});

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}
