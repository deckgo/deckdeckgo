import {firestore} from 'firebase-admin';

export interface AssetData {
    data?: string[];
    images?: string[];

    created_at?: firestore.Timestamp;
    updated_at?: firestore.Timestamp;
}

export interface Asset {
    id: string;
    ref: firestore.DocumentReference;
    data: AssetData;
}
