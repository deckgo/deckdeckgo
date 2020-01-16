import * as admin from 'firebase-admin';
import * as functions from 'firebase-functions';

import {JSDOM} from 'jsdom';

import {Asset, AssetData} from '../../../model/asset';
import {SlideData} from '../../../model/slide';

import {Resources} from '../../../utils/resources';

export function findAsset(deckId: string): Promise<Asset | undefined> {
    return new Promise<Asset | undefined>(async (resolve, reject) => {
        try {
            const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/assets/${deckId}`).get();

            if (!snapshot.exists) {
                resolve(undefined);
                return;
            }

            resolve({
                id: snapshot.id,
                ref: snapshot.ref,
                data: snapshot.data() as AssetData
            });
        } catch (err) {
            reject(err);
        }
    })
}

export function createAsset(deckId: string, assetData: AssetData): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        const now: admin.firestore.Timestamp = admin.firestore.Timestamp.now();
        assetData.updated_at = now;
        assetData.created_at = now;

        try {
            await createOrUpdate(deckId, assetData);

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}

export function updateAsset(deckId: string, assetData: AssetData): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        const now: admin.firestore.Timestamp = admin.firestore.Timestamp.now();
        assetData.updated_at = now;

        try {
            await createOrUpdate(deckId, assetData);

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}

export function deleteAsset(deckId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        try {
            if (!deckId || deckId === undefined || deckId === '') {
                resolve();
                return;
            }

            const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/assets/`);
            const doc: admin.firestore.DocumentReference = collectionRef.doc(deckId);

            await doc.delete();

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}

function createOrUpdate(deckId: string, assetData: AssetData): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/assets/${deckId}/`);

        try {
            await documentReference.set(assetData, {merge: true});

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}

export function findImages(slideData: SlideData): Promise<HTMLElement[] | undefined> {
    return new Promise<HTMLElement[] | undefined>((resolve) => {
        if (!slideData || !slideData.content || slideData.content === undefined || slideData.content === '') {
            resolve(undefined);
            return;
        }

        const dom: JSDOM = new JSDOM(`<!DOCTYPE html><deckgo-deck>${slideData.content}</deckgo-deck>`);

        const images: NodeListOf<HTMLElement> = dom.window.document.querySelectorAll('deckgo-lazy-img');

        resolve(images && images.length > 0 ? Array.from(images) : undefined);
    });
}

export function extractAssetPath(path: string | null): Promise<string | undefined> {
    return new Promise<string|undefined>((resolve) => {
        if (!path || path === undefined || path === '') {
            resolve(undefined);
            return;
        }

        const storageUrl: string = functions.config().storage.url;

        if (path.indexOf(storageUrl) <= -1) {
            resolve(undefined);
            return;
        }

        resolve(decodeURIComponent(path.replace(storageUrl, '').replace(`?${Resources.Constants.STORAGE.MEDIA_PARAM}`, '')))
    });
}

export

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
