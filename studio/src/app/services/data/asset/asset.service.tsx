import {firebase} from '@firebase/app';
import '@firebase/firestore';

import {Asset, AssetData} from '../../../models/data/asset';

export class AssetService {

    private static instance: AssetService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!AssetService.instance) {
            AssetService.instance = new AssetService();
        }
        return AssetService.instance;
    }

    create(deckId: string, assetData: AssetData): Promise<Asset> {
        return new Promise<Asset>(async (resolve, reject) => {
            const firestore: firebase.firestore.Firestore = firebase.firestore();

            const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
            assetData.created_at = now;
            assetData.updated_at = now;

            try {
                await firestore.collection('assets').doc(deckId).set(assetData);

                resolve({
                    id: deckId,
                    data: assetData
                });
            } catch (err) {
                reject(err);
            }
        });
    }

    get(deckId: string): Promise<Asset | undefined> {
        return new Promise<Asset | undefined>(async (resolve, reject) => {
            const firestore: firebase.firestore.Firestore = firebase.firestore();

            try {
                const snapshot: firebase.firestore.DocumentSnapshot = await firestore.collection('assets').doc(deckId).get();

                if (!snapshot.exists) {
                    resolve(undefined);
                    return;
                }

                const asset: AssetData = snapshot.data() as AssetData;

                resolve({
                    id: snapshot.id,
                    data: asset
                });
            } catch (err) {
                reject(err);
            }
        });
    }

    update(deckId: string, asset: Asset): Promise<Asset> {
        return new Promise<Asset>(async (resolve, reject) => {
            const firestore: firebase.firestore.Firestore = firebase.firestore();

            const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
            asset.data.updated_at = now;

            try {
                await firestore.collection('assets').doc(deckId).set(asset.data, {merge: true});

                resolve(asset);
            } catch (err) {
                reject(err);
            }
        });
    }
}
