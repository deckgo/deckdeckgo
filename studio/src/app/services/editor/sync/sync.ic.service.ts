import {Identity} from '@dfinity/agent';

import authStore from '../../../stores/auth.store';
import syncStore from '../../../stores/sync.store';

import {Deck} from '../../../models/data/deck';
import {Slide} from '../../../models/data/slide';

import {SyncData, SyncDataDeck, SyncDataSlide} from '../../../types/editor/sync';

import {idlFactory as DeckFactory} from '../../../functions/decks/decks.utils.did';
import {idlFactory as SlideFactory} from '../../../functions/slides/slides.utils.did';
import {_SERVICE as DeckActor} from '../../../functions/decks/decks.did';
import {_SERVICE as SlideActor} from '../../../functions/slides/slides.did';

import {internetComputer} from '../../../utils/core/environment.utils';
import {createActor} from '../../../utils/core/ic.utils';
import {CanisterUtils} from '../../../utils/editor/canister.utils';

import {SyncService} from './sync.service';
import {AuthFactoryService} from '../../auth/auth.factory.service';
import {AuthIcService} from '../../auth/auth.ic.service';

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
        header: CanisterUtils.toNullable<string>(deck.data.header)
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
        content: CanisterUtils.toNullable<string>(slide.data.content),
        template: slide.data.template,
        scope: CanisterUtils.toNullable<string>(slide.data.scope),
        attributes: CanisterUtils.prepareAttributes(slide.data.attributes),
        created_at: CanisterUtils.toNullableTimestamp(slide.data.created_at as Date | undefined),
        updated_at: CanisterUtils.toNullableTimestamp(slide.data.updated_at as Date | undefined)
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

    const promises: Promise<void>[] = deleteSlides.map(({slideId}: SyncDataSlide) => this.deleteSlide({slideId, slideActor}));
    await Promise.all(promises);
  }

  private async deleteSlide({slideId, slideActor}: {slideId: string; slideActor: SlideActor}) {
    if (!slideId) {
      return;
    }

    await slideActor.del(slideId);
  }

  private createDeckActor({identity}: {identity: Identity}): Promise<DeckActor> {
    return createActor<DeckActor>({canisterId: process.env.DECKS_CANISTER_ID, idlFactory: DeckFactory, identity});
  }

  private createSlideActor({identity}: {identity: Identity}): Promise<SlideActor> {
    return createActor<SlideActor>({canisterId: process.env.SLIDES_CANISTER_ID, idlFactory: SlideFactory, identity});
  }
}
