import {firebase} from '@firebase/app';

import {get, set} from 'idb-keyval';

import {Deck} from '../../../models/data/deck';

import {OfflineUtils} from '../../../utils/editor/offline.utils';

export class DeckOfflineService {
  private static instance: DeckOfflineService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!DeckOfflineService.instance) {
      DeckOfflineService.instance = new DeckOfflineService();
    }
    return DeckOfflineService.instance;
  }

  get(deckId: string): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      try {
        const deck: Deck = await get(`/decks/${deckId}`);

        resolve(deck);
      } catch (err) {
        reject(err);
      }
    });
  }

  update(deck: Deck): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      try {
        if (!deck || !deck.data) {
          reject('Invalid deck data');
          return;
        }

        const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
        deck.data.updated_at = now;

        if (deck.data.background && OfflineUtils.shouldAttributeBeCleaned(deck.data.background)) {
          deck.data.background = null;
        }

        deck.data.attributes = await OfflineUtils.cleanAttributes(deck.data.attributes);

        await set(`/decks/${deck.id}`, deck);

        resolve(deck);
      } catch (err) {
        reject(err);
      }
    });
  }
}
