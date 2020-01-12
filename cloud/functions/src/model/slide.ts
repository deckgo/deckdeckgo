import {firestore} from 'firebase-admin';

export interface Slide {
    id: string;
    ref: firestore.DocumentReference;
    // For simplicity reason cast to any as we don't want to redefine them as in studio
    data: any;
}
