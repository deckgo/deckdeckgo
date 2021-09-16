import {Deck, DeckData} from '@deckdeckgo/editor';

export class DeckFirebaseProvider {
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

  async create(deck: DeckData): Promise<Deck> {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {createDeck: createUserDeck} = await import(cdn);

    return createUserDeck(deck);
  }

  async update(deck: Deck): Promise<Deck> {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {updateDeck: updateUserDeck} = await import(cdn);

    return updateUserDeck(deck);
  }
}
