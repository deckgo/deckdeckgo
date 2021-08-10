import {Identity} from '@dfinity/agent';

import authStore from '../../../stores/auth.store';
import syncStore from '../../../stores/sync.store';

import {SyncData, SyncDataDeck, SyncDataSlide} from '../../../types/editor/sync';

import {_SERVICE as DecskActor} from '../../../canisters/decks/decks.did';

import {internetComputer} from '../../../utils/core/environment.utils';
import {createDecksActor} from '../../../utils/core/ic.deck.utils';

import {SyncService} from './sync.service';
import {DeckIcService} from '../../data/deck/deck.ic.service';
import {SlideIcService} from '../../data/slide/slide.ic.service';
import {AuthFactoryService} from '../../auth/auth.factory.service';
import {AuthIcService} from '../../auth/auth.ic.service';

// TODO: can we move this in a web worker? the IC SDK is compatible? => No with agent-js, manually probably

export class SyncIcService extends SyncService {
  private deckIcService: DeckIcService;
  private slideIcService: SlideIcService;

  constructor() {
    super();

    this.deckIcService = DeckIcService.getInstance();
    this.slideIcService = SlideIcService.getInstance();
  }

  // @Override
  async upload(syncData: SyncData | undefined) {
    try {
      if (!syncData) {
        return;
      }

      if (!authStore.state.loggedIn || !navigator.onLine) {
        return;
      }

      // TODO: fix me. does not work if no changes are made after sign in or coming back

      if (!this.isSyncPending()) {
        return;
      }

      if (!internetComputer()) {
        return;
      }

      const identity: Identity | undefined = (AuthFactoryService.getInstance() as AuthIcService).getIdentity();

      if (!identity) {
        return;
      }

      syncStore.state.sync = 'in_progress';

      const {updateDecks, updateSlides, deleteSlides} = syncData;

      const decksActor: DecskActor = await createDecksActor({identity});

      await this.uploadDecks({updateDecks, identity, decksActor});

      await this.uploadSlides({updateSlides, identity, decksActor});

      await this.deleteSlides({deleteSlides, identity, decksActor});

      // TODO: handle delete decks here?

      await this.clean(syncData);
    } catch (err) {
      syncStore.state.sync = 'error';
      console.error(err);
    }
  }

  private async uploadDecks({updateDecks, identity, decksActor}: {updateDecks: SyncDataDeck[] | undefined; identity: Identity; decksActor: DecskActor}) {
    if (!updateDecks || updateDecks.length <= 0) {
      return;
    }

    const promises: Promise<void>[] = updateDecks.map(({deck}: SyncDataDeck) => this.deckIcService.uploadDeck({deck, decksActor, identity}));
    await Promise.all(promises);
  }

  private async uploadSlides({updateSlides, identity, decksActor}: {updateSlides: SyncDataSlide[] | undefined; identity: Identity; decksActor: DecskActor}) {
    if (!updateSlides || updateSlides.length <= 0) {
      return;
    }

    const promises: Promise<void>[] = updateSlides.map(({slide, deckId}: SyncDataSlide) =>
      this.slideIcService.uploadSlide({slide, deckId, decksActor, identity})
    );
    await Promise.all(promises);
  }

  private async deleteSlides({deleteSlides, identity, decksActor}: {deleteSlides: SyncDataSlide[] | undefined; identity: Identity; decksActor: DecskActor}) {
    if (!deleteSlides || deleteSlides.length <= 0) {
      return;
    }

    const promises: Promise<void>[] = deleteSlides.map(({deckId, slideId}: SyncDataSlide) =>
      this.slideIcService.deleteSlide({slideId, deckId, identity, decksActor})
    );
    await Promise.all(promises);
  }
}
