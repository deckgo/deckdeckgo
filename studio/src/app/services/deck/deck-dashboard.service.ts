import firebase from 'firebase/app';
import 'firebase/firestore';

import {Deck, DeckData} from '../../models/data/deck';

import { DeckOnlineService } from '../data/deck/deck.online.service';

export interface DeckDashboardCloneResult {
  from: Deck;
  to: Deck;
}

export class DeckDashboardService {
  private static instance: DeckDashboardService;

  private deckOnlineService: DeckOnlineService;

  private constructor() {
    this.deckOnlineService = DeckOnlineService.getInstance();
  }

  static getInstance() {
    if (!DeckDashboardService.instance) {
      DeckDashboardService.instance = new DeckDashboardService();
    }
    return DeckDashboardService.instance;
  }

  clone(deck: Deck): Promise<DeckDashboardCloneResult> {
    return new Promise<DeckDashboardCloneResult>(async (resolve, reject) => {
      try {
        const clone: Deck = await this.cloneDeck(deck);

        deck.data.clone = {
          deck_id_to: clone.id
        };

        const updatedDeck: Deck = await this.deckOnlineService.update(deck);

        resolve({
          from: updatedDeck,
          to: clone
        });
      } catch (err) {
        reject(err);
      }
    });
  }

  private cloneDeck(deck: Deck): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      try {
        let clone: DeckData = {...deck.data};

        clone.clone = {
          deck_id_from: deck.id
        };

        delete clone['slides'];
        delete clone['api_id'];
        delete clone['meta'];

        const createdDeck: Deck = await this.deckOnlineService.create(clone);

        resolve(createdDeck);
      } catch (err) {
        reject(err);
      }
    });
  }

  snapshot(deck: Deck, updateFunction: Function): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!deck || !deck.id) {
        resolve();
        return;
      }

      const firestore: firebase.firestore.Firestore = firebase.firestore();
      const unsubscribe = firestore
        .collection('decks')
        .doc(deck.id)
        .onSnapshot((deckSnapshot: firebase.firestore.DocumentSnapshot<DeckData>) => {
          updateFunction(
            {
              id: deckSnapshot.id,
              data: deckSnapshot.data()
            },
            unsubscribe
          );
        });

      resolve();
    });
  }
}
