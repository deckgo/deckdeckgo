import * as admin from 'firebase-admin';

import {Slide} from '../../../model/slide';

export function cloneSlides(deckIdTo: string, slides: Slide[] | null): Promise<string[] | undefined> {
    return new Promise<string[] | undefined>(async (resolve, reject) => {
        try {
            if (!slides || slides.length <= 0) {
                resolve(undefined);
                return;
            }

            const promises: Promise<string>[] = [];
            slides.forEach((slide: Slide) => {
                promises.push(cloneSlide(deckIdTo, slide));
            });

            if (promises && promises.length > 0) {
                const slideIds: string[] = await Promise.all(promises);
                resolve(slideIds);
                return;
            }

            resolve(undefined);
        } catch (err) {
            reject(err);
        }
    });
}

export function updateCloneData(deckId: string, slidesIds?: string[] | undefined): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        try {
            if (!deckId || deckId === undefined || deckId === '') {
                resolve();
                return;
            }

            const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/decks/${deckId}/`);

            let updateData: any = {
                clone: admin.firestore.FieldValue.delete(),
                updated_at: admin.firestore.Timestamp.now()
            };

            if (slidesIds !== undefined && slidesIds.length > 0) {
                updateData['slides'] = slidesIds;
            }

            await documentReference.set(updateData, {merge: true});

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}

function cloneSlide(deckIdTo: string, slide: Slide): Promise<string> {
    return new Promise<string>(async (resolve, reject) => {
        const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/decks/${deckIdTo}/slides/`);

        const slideData = {...slide.data};

        const now: admin.firestore.Timestamp = admin.firestore.Timestamp.now();
        slideData.created_at = now;
        slideData.updated_at = now;

        collectionRef.add(slideData).then(async (doc: admin.firestore.DocumentReference) => {
            resolve(doc.id);
        }, (err) => {
            reject(err);
        });
    });
}
