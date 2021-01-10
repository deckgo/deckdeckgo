import firebase from 'firebase/app';
import 'firebase/firestore';

import {Deck, DeckData} from '../../../models/data/deck';

import {DeckOfflineService} from './deck.offline.service';
import {DeckOnlineService} from './deck.online.service';
import {OfflineService} from '../../editor/offline/offline.service';

export class DeckService {
  private static instance: DeckService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!DeckService.instance) {
      DeckService.instance = new DeckService();
    }
    return DeckService.instance;
  }

  create(deck: DeckData): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
      deck.created_at = now;
      deck.updated_at = now;

      firestore
        .collection('decks')
        .add(deck)
        .then(
          async (doc: firebase.firestore.DocumentReference) => {
            resolve({
              id: doc.id,
              data: deck,
            });
          },
          (err) => {
            reject(err);
          }
        );
    });
  }

  async get(deckId: string): Promise<Deck> {
    const offline: OfflineDeck = await OfflineService.getInstance().status();

    if (offline !== undefined) {
      return DeckOfflineService.getInstance().get(deckId);
    } else {
      return DeckOnlineService.getInstance().get(deckId);
    }
  }

  async update(deck: Deck): Promise<Deck> {
    const offline: OfflineDeck = await OfflineService.getInstance().status();

    if (offline !== undefined) {
      return DeckOfflineService.getInstance().update(deck);
    } else {
      return DeckOnlineService.getInstance().update(deck);
    }
  }

  getUserDecks(userId: string): Promise<Deck[]> {
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
            data: documentSnapshot.data() as DeckData,
          };
        });

        resolve(decks);
      } catch (err) {
        reject(err);
      }
    });
  }

  mergeDeck(deckId: string, newUserId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      if (!deckId || !newUserId) {
        resolve();
        return;
      }

      try {
        const deck: Deck = await this.get(deckId);

        if (!deck || !deck.data) {
          resolve();
          return;
        }

        deck.data.owner_id = newUserId;

        await this.update(deck);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

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
