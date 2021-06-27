import {Deck, DeckData} from '../../../models/data/deck';

import {DeckOfflineService} from './deck.offline.service';
import {DeckOnlineService} from './deck.online.service';

import authStore from '../../../stores/auth.store';

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

  // TODO: clean fields before online queries

  async create(deckData: DeckData): Promise<Deck> {
    const deck: Deck = await DeckOfflineService.getInstance().create(deckData);

    if (navigator.onLine && authStore.state.loggedIn) {
      await DeckOnlineService.getInstance().update(deck);
    }

    return deck;
  }

  async get(deckId: string): Promise<Deck> {
    return DeckOfflineService.getInstance().get(deckId);
  }

  async update(deck: Deck): Promise<Deck> {
    const updatedDeck: Deck = await DeckOfflineService.getInstance().update(deck);

    if (navigator.onLine && authStore.state.loggedIn) {
      await DeckOnlineService.getInstance().update(deck);
    }

    return updatedDeck;
  }
}
