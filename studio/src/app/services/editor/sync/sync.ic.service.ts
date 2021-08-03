import {Identity} from '@dfinity/agent';

import authStore from '../../../stores/auth.store';
import syncStore from '../../../stores/sync.store';

import {SyncData, SyncDataDeck, SyncDataSlide} from '../../../types/editor/sync';

import {_SERVICE as DeckActor} from '../../../functions/decks/decks.did';
import {_SERVICE as SlideActor} from '../../../functions/slides/slides.did';

import {internetComputer} from '../../../utils/core/environment.utils';

import {SyncService} from './sync.service';
import {DeckIcService} from '../../data/deck/deck.ic.service';
import {SlideIcService} from '../../data/slide/slide.ic.service';
import {AuthFactoryService} from '../../auth/auth.factory.service';
import {AuthIcService} from '../../auth/auth.ic.service';

// TODO: can we move this in a web worker? the IC SDK is compatible?

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

      await this.uploadDecks({updateDecks, identity});

      await this.uploadSlides({updateSlides, identity});

      await this.deleteSlides({deleteSlides, identity});

      // TODO: handle delete decks here?

      await this.clean(syncData);
    } catch (err) {
      syncStore.state.sync = 'error';
      console.error(err);
    }
  }

  private async uploadDecks({updateDecks, identity}: {updateDecks: SyncDataDeck[] | undefined; identity: Identity}) {
    if (!updateDecks || updateDecks.length <= 0) {
      return;
    }

    const deckActor: DeckActor = await this.deckIcService.createActor({identity});

    const promises: Promise<void>[] = updateDecks.map(({deck}: SyncDataDeck) => this.deckIcService.uploadDeck({deck, deckActor}));
    await Promise.all(promises);
  }

  private async uploadSlides({updateSlides, identity}: {updateSlides: SyncDataSlide[] | undefined; identity: Identity}) {
    if (!updateSlides || updateSlides.length <= 0) {
      return;
    }

    const slideActor: SlideActor = await this.slideIcService.createActor({identity});

    const promises: Promise<void>[] = updateSlides.map(({slide, deckId}: SyncDataSlide) => this.slideIcService.uploadSlide({slide, deckId, slideActor}));
    await Promise.all(promises);
  }

  private async deleteSlides({deleteSlides, identity}: {deleteSlides: SyncDataSlide[] | undefined; identity: Identity}) {
    if (!deleteSlides || deleteSlides.length <= 0) {
      return;
    }

    const slideActor: SlideActor = await this.slideIcService.createActor({identity});

    const promises: Promise<void>[] = deleteSlides.map(({slideId}: SyncDataSlide) => this.slideIcService.deleteSlide({slideId, slideActor}));
    await Promise.all(promises);
  }
}
