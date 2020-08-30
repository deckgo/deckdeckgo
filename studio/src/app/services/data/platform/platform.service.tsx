import * as firebase from 'firebase/app';
import 'firebase/firestore';

import errorStore from '../../../stores/error.store';
import authStore from '../../../stores/auth.store';
import platformStore from '../../../stores/platform.store';
import deckStore from '../../../stores/deck.store';

import {Platform} from '../../../models/data/platform';
import {Deck} from '../../../models/data/deck';
import {PlatformDeckData} from '../../../models/data/platform-deck';

export class PlatformService {
  private static instance: PlatformService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!PlatformService.instance) {
      PlatformService.instance = new PlatformService();
    }
    return PlatformService.instance;
  }

  async merge(platform: Platform) {
    if (!platform) {
      return;
    }

    try {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      platform.data.updated_at = firebase.firestore.Timestamp.now();

      await firestore.collection('platforms').doc(platform.id).set(platform.data, {merge: true});
    } catch (err) {
      errorStore.state.error = 'GitHub platform information not properly set up.';
    }
  }

  snapshotPlatformDeck(): Promise<() => void | undefined> {
    return new Promise<() => void | undefined>((resolve) => {
      const deck: Deck = deckStore.state.deck;

      if (!deck || !deck.id || !deck.data || !deck.data.owner_id) {
        platformStore.reset();

        resolve(undefined);
        return;
      }

      if (!authStore.state.gitHub) {
        platformStore.reset();

        resolve(undefined);
        return;
      }

      const firestore: firebase.firestore.Firestore = firebase.firestore();
      const unsubscribe = firestore
        .collection(`platforms/${deck.data.owner_id}/decks`)
        .doc(deck.id)
        .onSnapshot((deckSnapshot: firebase.firestore.DocumentSnapshot<PlatformDeckData>) => {
          platformStore.state.platformDeck = {
            id: deckSnapshot.id,
            data: deckSnapshot.data(),
          };
        });

      resolve(unsubscribe);
    });
  }
}
