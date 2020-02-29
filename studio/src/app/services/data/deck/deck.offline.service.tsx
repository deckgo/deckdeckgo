import {get} from 'idb-keyval';

import {Deck} from '../../../models/data/deck';

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
}
