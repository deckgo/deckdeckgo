import {v4 as uuid} from 'uuid';

import {get, set} from 'idb-keyval';

import {Deck, DeckAttributes, DeckData} from '../../../models/data/deck';

import {OfflineUtils} from '../../../utils/editor/offline.utils';
import {FirestoreUtils} from '../../../utils/editor/firestore.utils';
import { syncUpdateDeck } from '../../../utils/editor/sync.utils';

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

  create(deckData: DeckData): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      try {
        const deckId: string = uuid();

        const now: Date = new Date();

        const deck: Deck = {
          id: deckId,
          data: {
            ...deckData,
            updated_at: now,
            created_at: now
          }
        };

        await set(`/decks/${deckId}`, deck);

        await syncUpdateDeck(deckId);

        resolve(deck);
      } catch (err) {
        reject(err);
      }
    });
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

        deck.data.updated_at = new Date();

        if (deck.data.background && FirestoreUtils.shouldAttributeBeCleaned(deck.data.background)) {
          deck.data.background = null;
        }

        if (deck.data.header && FirestoreUtils.shouldAttributeBeCleaned(deck.data.header)) {
          deck.data.header = null;
        }

        if (deck.data.footer && FirestoreUtils.shouldAttributeBeCleaned(deck.data.footer)) {
          deck.data.footer = null;
        }

        deck.data.attributes = (await OfflineUtils.cleanAttributes(deck.data.attributes)) as DeckAttributes;

        await set(`/decks/${deck.id}`, deck);

        await syncUpdateDeck(deck.id);

        resolve(deck);
      } catch (err) {
        reject(err);
      }
    });
  }
}
