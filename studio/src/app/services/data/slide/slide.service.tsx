import {firebase} from '@firebase/app';
import '@firebase/firestore';

import {Slide, SlideData} from '../../../models/data/slide';

export class SlideService {

    private static instance: SlideService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!SlideService.instance) {
            SlideService.instance = new SlideService();
        }
        return SlideService.instance;
    }

    create(deckId: string, slide: SlideData): Promise<Slide> {
        return new Promise<Slide>(async (resolve, reject) => {
            const firestore: firebase.firestore.Firestore = firebase.firestore();

            const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
            slide.created_at = now;
            slide.updated_at = now;

            firestore.collection(`/decks/${deckId}/slides`).add(slide).then(async (doc: firebase.firestore.DocumentReference) => {
                resolve({
                    id: doc.id,
                    data: slide
                });
            }, (err) => {
                reject(err);
            });
        });
    }

    update(deckId: string, slide: Slide): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            const firestore: firebase.firestore.Firestore = firebase.firestore();

            const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
            slide.data.updated_at = now;

            try {
                await firestore.collection(`/decks/${deckId}/slides`).doc(slide.id).set(slide.data, {merge: true});

                resolve();
            } catch (err) {
                reject(err);
            }
        });
    }

    delete(deckId: string, slideId: string): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                const firestore: firebase.firestore.Firestore = firebase.firestore();

                await firestore.collection(`/decks/${deckId}/slides`).doc(slideId).delete();

                resolve();
            } catch (err) {
                reject(err);
            }
        });
    }

    get(deckId: string, slideId: string): Promise<Slide> {
        return new Promise<Slide>(async (resolve, reject) => {
            const firestore: firebase.firestore.Firestore = firebase.firestore();

            try {
                const snapshot: firebase.firestore.DocumentSnapshot = await firestore.collection(`/decks/${deckId}/slides`).doc(slideId).get();

                if (!snapshot.exists) {
                    reject('Slide not found');
                    return;
                }

                const slide: SlideData = snapshot.data() as SlideData;

                resolve({
                    id: snapshot.id,
                    data: slide
                });
            } catch (err) {
                reject(err);
            }
        });
    }

    getSlides(deckId: string): Promise<Slide[]> {
        return new Promise<Slide[]>(async (resolve, reject) => {
            try {
                const firestore: firebase.firestore.Firestore = firebase.firestore();

                const snapshot: firebase.firestore.QuerySnapshot = await firestore.collection(`/decks/${deckId}/slides`)
                    .orderBy('created_at', 'asc').get();

                const slides: Slide[] = snapshot.docs.map((documentSnapshot: firebase.firestore.QueryDocumentSnapshot) => {
                    return {
                        id: documentSnapshot.id,
                        data: documentSnapshot.data() as SlideData
                    }
                });

                resolve(slides);
            } catch (err) {
                reject(err);
            }
        });
    }
}
