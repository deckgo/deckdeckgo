import {firestore} from 'firebase-admin';

export interface SlideData {
    content?: string;
    // For simplicity reason only typed as "string", see slide.tsx for details
    template: string;
    // For simplicity reason only typed as "any", see slide.tsx for details
    attributes?: any;

    api_id?: string;

    created_at?: firestore.Timestamp;
    updated_at?: firestore.Timestamp;
}

export interface Slide {
    id: string;
    ref: firestore.DocumentReference;
    data: SlideData;
}
