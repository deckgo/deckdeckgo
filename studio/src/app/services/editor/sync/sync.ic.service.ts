import {decks as deckIc} from '../../../functions/decks/index';

import authStore from '../../../stores/auth.store';

import {SyncData, SyncDataDeck} from '../../../types/editor/sync';

import {internetComputerEnabled} from '../../../utils/core/environment.utils';

// TODO: can we move this in a web worker? the IC SDK is compatible?

export class SyncIcService {
  private static instance: SyncIcService;

  private constructor() {

  }

  static getInstance(): SyncIcService {
    if (!SyncIcService.instance) {
      SyncIcService.instance = new SyncIcService();
    }
    return SyncIcService.instance;
  }

  public async sync(syncData: SyncData) {
    if (!internetComputerEnabled()) {
      return;
    }

    const {updateDecks} = syncData;

    if (!updateDecks || updateDecks.length <= 0) {
      return;
    }

    const promises: Promise<void>[] = updateDecks.map((deck: SyncDataDeck) => this.uploadDeckIC(deck));
    await Promise.all(promises);
  };


  private async uploadDeckIC({deck}: SyncDataDeck) {
    if (!deck) {
      return;
    }

    // TODO: Locally error -> Fail to verify certificate - https://forum.dfinity.org/t/fail-to-verify-certificate-in-development-update-calls/4078/14

    // TODO: typescript declaration not correctly generated by SDK -> https://forum.dfinity.org/t/fail-to-verify-certificate-in-development-update-calls/4078/14

    await deckIc.set({
      id: deck.id,
      data: {
        name: deck.data.name,
        owner_id: authStore.state.authUser.uid,
        header: []
      }
    });

    // TODO: remove, just for test
    console.log('Deck IC Get:', await deckIc.get(deck.id));
  };
}

