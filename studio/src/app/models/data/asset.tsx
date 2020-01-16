export interface AssetData {
    owner_id: string;

    data?: string[];
    images?: string[];

    created_at?: firebase.firestore.Timestamp;
    updated_at?: firebase.firestore.Timestamp;
}

export interface Asset {
    id: string;
    data: AssetData;
}
