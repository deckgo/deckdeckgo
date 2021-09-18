import {Deck, UpdateDeck} from '@deckdeckgo/editor';

import {cloudProvider} from '../../../utils/core/providers.utils';

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

  async update(deck: Deck): Promise<Deck> {
    const {updateDeck: updateUserDeck}: {updateDeck: UpdateDeck} = await cloudProvider<{updateDeck: UpdateDeck}>();

    return updateUserDeck(deck);
  }
}
