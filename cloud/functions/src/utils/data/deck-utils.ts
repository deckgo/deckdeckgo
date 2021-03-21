import * as admin from 'firebase-admin';

import {format} from 'date-fns';

import {Deck, DeckData} from '../../model/data/deck';
import {fonts, GoogleFont} from '../../request/utils/google-fonts-utils';

export function findDeck(deckId: string): Promise<Deck> {
  return new Promise<Deck>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/decks/${deckId}/`).get();

      if (!snapshot.exists) {
        reject('Deck not found');
        return;
      }

      const deckData: DeckData = snapshot.data() as DeckData;

      resolve({
        id: snapshot.id,
        ref: snapshot.ref,
        data: deckData,
      });
    } catch (err) {
      reject(err);
    }
  });
}

export function updateDeck(deckId: string, deckData: Partial<DeckData>): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!deckId || deckId === undefined || deckId === '') {
        reject('No deck ID provided to update.');
        return;
      }

      const data: Partial<DeckData> = {...deckData};
      data.updated_at = admin.firestore.Timestamp.now();

      const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/decks/${deckId}`);

      await documentReference.set(data, {merge: true});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

export function createDeck(ownerId: string, fontFamily?: string): Promise<Deck> {
  return new Promise<Deck>(async (resolve, reject) => {
    try {
      const now: admin.firestore.Timestamp = admin.firestore.Timestamp.now();

      let data: DeckData = {
        name: `Presentation ${format(new Date(), 'MMM d yyyy HH-mm-ss')}`,
        owner_id: ownerId,
        created_at: now,
        updated_at: now,
      };

      const font: GoogleFont | undefined = fonts.find((font: GoogleFont) => font.name === fontFamily && fontFamily !== undefined);
      if (font) {
        data = {
          ...data,
          attributes: {
            style: `font-family: ${font.family};`,
          },
        };
      }

      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection('/decks/');
      const doc: admin.firestore.DocumentReference = await collectionRef.add(data);

      resolve({
        id: doc.id,
        ref: doc,
        data: data,
      });
    } catch (err) {
      reject(err);
    }
  });
}

export function findPublishedDecks(): Promise<Deck[]> {
  return new Promise<Deck[]>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.QuerySnapshot = await admin
        .firestore()
        .collection('decks')
        .where('meta.feed', '==', true)
        .orderBy('meta.published_at', 'desc')
        .get();

      if (!snapshot || !snapshot.docs) {
        resolve([]);
        return;
      }

      const decks: Deck[] = snapshot.docs.map((doc) => {
        const id = doc.id;
        const ref = doc.ref;

        return {
          id: id,
          ref: ref,
          data: doc.data(),
        } as Deck;
      });

      resolve(decks);
    } catch (err) {
      reject(err);
    }
  });
}
