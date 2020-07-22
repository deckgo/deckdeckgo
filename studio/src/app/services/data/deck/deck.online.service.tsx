import * as firebase from 'firebase/app';
import 'firebase/firestore';

import {Deck, DeckData} from '../../../models/data/deck';

export class DeckOnlineService {
  private static instance: DeckOnlineService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!DeckOnlineService.instance) {
      DeckOnlineService.instance = new DeckOnlineService();
    }
    return DeckOnlineService.instance;
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
          data: deck,
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
      deck.data.updated_at = now;

      try {
        await firestore.collection('decks').doc(deck.id).set(deck.data, {merge: true});

        // Fetch newly persisted deck (clean firebase.firestore.FieldValue.delete())
        const updatedDeck: Deck = await this.get(deck.id);

        resolve(updatedDeck);
      } catch (err) {
        reject(err);
      }
    });
  }
}
