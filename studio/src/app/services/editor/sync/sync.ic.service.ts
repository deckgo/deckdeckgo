import {Identity} from '@dfinity/agent';

import authStore from '../../../stores/auth.store';
import syncStore from '../../../stores/sync.store';

import {Deck} from '../../../models/data/deck';

import {SyncData, SyncDataDeck, SyncDataSlide} from '../../../types/editor/sync';

import {idlFactory as DeckFactory} from '../../../functions/decks/decks.utils.did';
import {idlFactory as SlideFactory} from '../../../functions/slides/slides.utils.did';
import {_SERVICE as DeckActor} from '../../../functions/decks/decks.did';
import {_SERVICE as SlideActor} from '../../../functions/slides/slides.did';

import {internetComputer} from '../../../utils/core/environment.utils';
import {createActor} from '../../../utils/core/ic.utils';

import {SyncService} from './sync.service';
import {AuthFactoryService} from '../../auth/auth.factory.service';
import {AuthIcService} from '../../auth/auth.ic.service';
import {Slide} from '../../../models/data/slide';

// TODO: can we move this in a web worker? the IC SDK is compatible?

export class SyncIcService extends SyncService {
  // @Override
  async upload(syncData: SyncData | undefined) {
    try {
      if (!syncData) {
        return;
      }

      if (!authStore.state.loggedIn || !navigator.onLine) {
        return;
      }

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

    const deckActor: DeckActor = await this.createDeckActor({identity});

    const promises: Promise<void>[] = updateDecks.map(({deck}: SyncDataDeck) => this.uploadDeck({deck, deckActor}));
    await Promise.all(promises);
  }

  private async uploadDeck({deck, deckActor}: {deck: Deck; deckActor: DeckActor}) {
    if (!deck) {
      return;
    }

    await deckActor.set({
      id: deck.id,
      data: {
        name: deck.data.name,
        header: this.toNullString(deck.data.header)
      }
    });

    // TODO: remove, just for test
    console.log('Deck IC Get:', await deckActor.get(deck.id));
  }

  private async uploadSlides({updateSlides, identity}: {updateSlides: SyncDataSlide[] | undefined; identity: Identity}) {
    if (!updateSlides || updateSlides.length <= 0) {
      return;
    }

    const slideActor: SlideActor = await this.createSlideActor({identity});

    const promises: Promise<void>[] = updateSlides.map(({slide}: SyncDataSlide) => this.uploadSlide({slide, slideActor}));
    await Promise.all(promises);
  }

  private async uploadSlide({slide, slideActor}: {slide: Slide; slideActor: SlideActor}) {
    if (!slide) {
      return;
    }

    await slideActor.set({
      id: slide.id,
      data: {
        content: this.toNullString(slide.data.content)
      }
    });

    // TODO: remove, just for test
    console.log('Slide IC Get:', await slideActor.get(slide.id));
  }

  private async deleteSlides({deleteSlides, identity}: {deleteSlides: SyncDataSlide[] | undefined; identity: Identity}) {
    if (!deleteSlides || deleteSlides.length <= 0) {
      return;
    }

    const slideActor: SlideActor = await this.createSlideActor({identity});

    const promises: Promise<void>[] = deleteSlides.map(({slide}: SyncDataSlide) => this.deleteSlide({slide, slideActor}));
    await Promise.all(promises);
  }

  private async deleteSlide({slide, slideActor}: {slide: Slide; slideActor: SlideActor}) {
    if (!slide) {
      return;
    }

    await slideActor.del(slide.id);

    // TODO: remove, just for test
    console.log('Slide IC Deleted:', await slideActor.get(slide.id));
  }

  // TODO: typescript declaration not correctly generated by SDK -> https://forum.dfinity.org/t/fail-to-verify-certificate-in-development-update-calls/4078/14
  private toNullString(value?: string): [] | [string] {
    return value ? [value] : [];
  }

  private createDeckActor({identity}: {identity: Identity}): Promise<DeckActor> {
    return createActor<DeckActor>({canisterId: process.env.DECKS_CANISTER_ID, idlFactory: DeckFactory, identity});
  }

  private createSlideActor({identity}: {identity: Identity}): Promise<SlideActor> {
    return createActor<SlideActor>({canisterId: process.env.SLIDES_CANISTER_ID, idlFactory: SlideFactory, identity});
  }
}
