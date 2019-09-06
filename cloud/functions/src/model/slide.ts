import {firestore} from 'firebase-admin';

export interface Slide {
    id: string;
    ref: firestore.DocumentReference;
}
