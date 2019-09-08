import * as admin from 'firebase-admin';

import {Deck, DeckData} from '../../model/deck';
import {Slide} from '../../model/slide';

interface DeckSlides {
    deckId: string;
    slides: Slide[] | null;
}

export async function deleteDecksSlides(userRecord: admin.auth.UserRecord) {
    if (!userRecord || !userRecord.uid || userRecord.uid === undefined || userRecord.uid === '') {
        return;
    }

    try {
        const userId: string = userRecord.uid;

        const decks: Deck[] | null = await findDecks(userId);

        if (!decks || decks.length <= 0) {
            return;
        }

        const decksSlides: (DeckSlides | null)[] = await findAllSlides(decks);

        if (!decksSlides || decksSlides.length <= 0) {
            await deleteDecks(decks);
            return;
        }

        await deleteAllSlides(decksSlides);

        await deleteDecks(decks);

    } catch (err) {
        console.error(err);
    }
}

function findDecks(userId: string): Promise<Deck[] | null> {
    return new Promise<Deck[] | null>(async (resolve, reject) => {
        try {
            const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection('/decks/');

            const snapShot: admin.firestore.QuerySnapshot = await collectionRef
                .where('owner_id', '==', userId)
                .get();

            if (snapShot && snapShot.docs && snapShot.docs.length > 0) {
                const decks: Deck[] = snapShot.docs.map((doc) => {
                    const data: Object = doc.data() as DeckData;
                    const id = doc.id;
                    const ref = doc.ref;

                    return {
                        id: id,
                        ref: ref,
                        data: data
                    } as Deck;
                });

                resolve(decks);
            } else {
                resolve(null);
            }
        } catch (err) {
            reject(err);
        }
    })
}

function findSlides(deckId: string): Promise<DeckSlides | null> {
    return new Promise<DeckSlides | null>(async (resolve, reject) => {
        try {
            if (!deckId || deckId === undefined || deckId === '') {
                resolve();
                return;
            }

            const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/decks/${deckId}/slides/`);

            const snapShot: admin.firestore.QuerySnapshot = await collectionRef.get();

            if (snapShot && snapShot.docs && snapShot.docs.length > 0) {
                const slides: Slide[] = snapShot.docs.map((doc) => {
                    const id = doc.id;
                    const ref = doc.ref;

                    return {
                        id: id,
                        ref: ref
                    } as Slide;
                });

                resolve({
                    deckId: deckId,
                    slides: slides
                });
            } else {
                resolve(null);
            }
        } catch (err) {
            reject(err);
        }
    })
}

function findAllSlides(decks: Deck[]): Promise<(DeckSlides | null)[]> {
    return new Promise<(DeckSlides | null)[]>(async (resolve, reject) => {
        try {
            const promises: Promise<DeckSlides | null>[] = [];
            decks.forEach((deck: Deck) => {
                promises.push(findSlides(deck.id));
            });

            if (!promises || promises.length <= 0) {
                resolve([]);
                return;
            }

            const slides: (DeckSlides | null)[] = await Promise.all(promises);
            resolve(slides);
        } catch (err) {
            reject(err);
        }
    });
}

function deleteAllSlides(decksSlides: (DeckSlides | null)[]): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        try {
            if (!decksSlides || decksSlides.length <= 0) {
                resolve();
                return;
            }

            const promises: Promise<void>[] = [];
            decksSlides.forEach((deckSlides: DeckSlides | null) => {
                if (deckSlides) {
                    promises.push(deleteSlides(deckSlides.deckId, deckSlides.slides));
                }
            });

            if (promises && promises.length > 0) {
                await Promise.all(promises);
            }

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}

function deleteSlides(deckId: string, slides: Slide[] | null): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        try {
            if (!slides || slides.length <= 0) {
                resolve();
                return;
            }

            const promises: Promise<void>[] = [];
            slides.forEach((slide: Slide) => {
                promises.push(deleteSlide(deckId, slide));
            });

            if (promises && promises.length > 0) {
                await Promise.all(promises);
            }

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}

function deleteSlide(deckId: string, slide: Slide): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        try {
            if (!slide || !slide.id || slide.id === undefined || slide.id === '') {
                resolve();
                return;
            }

            const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/decks/${deckId}/slides/`);
            const doc: admin.firestore.DocumentReference = collectionRef.doc(slide.id);

            await doc.delete();

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}

function deleteDecks(decks: Deck[]): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        try {
            if (!decks || decks.length <= 0) {
                resolve();
                return;
            }

            const promises: Promise<void>[] = [];
            decks.forEach((deck: Deck) => {
                promises.push(deleteDeck(deck));
            });

            if (promises && promises.length > 0) {
                await Promise.all(promises);
            }

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}

function deleteDeck(deck: Deck): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
        try {
            if (!deck || !deck.id || deck.id === undefined || deck.id === '') {
                resolve();
                return;
            }

            const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/decks/`);
            const doc: admin.firestore.DocumentReference = collectionRef.doc(deck.id);

            await doc.delete();

            resolve();
        } catch (err) {
            reject(err);
        }
    });
}
