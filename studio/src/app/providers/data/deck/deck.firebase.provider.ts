import firebase from 'firebase/app';
import 'firebase/firestore';

import {Deck, DeckData} from '@deckdeckgo/editor';

import {FirestoreUtils} from '../../../utils/editor/firestore.utils';

import {DeckProvider} from './deck.provider';

export class DeckFirebaseProvider implements DeckProvider {
  private static instance: DeckFirebaseProvider;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!DeckFirebaseProvider.instance) {
      DeckFirebaseProvider.instance = new DeckFirebaseProvider();
    }
    return DeckFirebaseProvider.instance;
  }

  create(deck: DeckData): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
      deck.created_at = now as unknown as Date;
      deck.updated_at = now as unknown as Date;

      firestore
        .collection('decks')
        .add(deck)
        .then(
          (doc: firebase.firestore.DocumentReference) => {
            resolve({
              id: doc.id,
              data: deck
            });
          },
          (err) => {
            reject(err);
          }
        );
    });
  }

  get(deckId: string): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      try {
        const snapshot: firebase.firestore.DocumentSnapshot = await firestore.collection('decks').doc(deckId).get();

        if (!snapshot.exists) {
          reject('Deck not found');
          return;
        }

        const deck: DeckData = snapshot.data() as DeckData;

        resolve({
          id: snapshot.id,
          data: deck
        });
      } catch (err) {
        reject(err);
      }
    });
  }

  update(deck: Deck): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
      deck.data.updated_at = now as unknown as Date;

      try {
        await firestore.collection('decks').doc(deck.id).set(deck.data, {merge: true});

        resolve(FirestoreUtils.filterDelete(deck));
      } catch (err) {
        reject(err);
      }
    });
  }

  // @Override
  entries(userId: string): Promise<Deck[]> {
    return new Promise<Deck[]>(async (resolve, reject) => {
      try {
        const firestore: firebase.firestore.Firestore = firebase.firestore();

        const snapshot: firebase.firestore.QuerySnapshot = await firestore
          .collection('decks')
          .where('owner_id', '==', userId)
          .orderBy('updated_at', 'desc')
          .get();

        const decks: Deck[] = snapshot.docs.map((documentSnapshot: firebase.firestore.QueryDocumentSnapshot) => {
          return {
            id: documentSnapshot.id,
            data: documentSnapshot.data() as DeckData
          };
        });

        resolve(decks);
      } catch (err) {
        reject(err);
      }
    });
  }

  // @Override
  delete(deckId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const firestore: firebase.firestore.Firestore = firebase.firestore();

        await firestore.collection('decks').doc(deckId).delete();

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }
}
