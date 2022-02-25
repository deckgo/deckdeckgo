import type {Deck} from '@deckdeckgo/editor';
import {Store} from './store';

export class DeckStore extends Store<Deck | null> {
  private static instance: DeckStore;

  private deck: Deck | null;

  private constructor() {
    super();
  }

  static getInstance() {
    if (!DeckStore.instance) {
      DeckStore.instance = new DeckStore();
    }
    return DeckStore.instance;
  }

  set(deck: Deck | null) {
    this.deck = deck;

    this.populate(deck);
  }

  get(): Deck | null {
    return this.deck;
  }

  override subscribe(callback: (data: Deck | null) => void): () => void {
    const unsubscribe: () => void = super.subscribe(callback);

    callback(this.deck);

    return unsubscribe;
  }
}
