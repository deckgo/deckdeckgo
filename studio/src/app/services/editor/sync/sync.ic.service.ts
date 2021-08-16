import {Identity} from '@dfinity/agent';
import {LocalStorage} from '@dfinity/auth-client';

import authStore from '../../../stores/auth.store';
import syncStore from '../../../stores/sync.store';

import {SyncData, SyncDataSlide} from '../../../types/editor/sync';

import {_SERVICE as DecskActor} from '../../../canisters/decks/decks.did';

import {internetComputer} from '../../../utils/core/environment.utils';
import {createDecksActor} from '../../../utils/core/ic.deck.utils';

import {SyncService} from './sync.service';
import {SlideIcService} from '../../data/slide/slide.ic.service';
import {AuthFactoryService} from '../../auth/auth.factory.service';
import {AuthIcService} from '../../auth/auth.ic.service';

import {uploadWorker} from '../../../workers/sync.ic.worker';

export class SyncIcService extends SyncService {
  private slideIcService: SlideIcService;

  constructor() {
    super();

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

      const storage: LocalStorage = new LocalStorage('ic-');

      const identityKey: string | null = await storage.get('identity');
      const delegationChain: string | null = await storage.get('delegation');

      await uploadWorker({
        identityKey,
        delegationChain,
        syncData,
        host: `${window.location}`
      });

      const {updateSlides, deleteSlides} = syncData;

      const decksActor: DecskActor = await createDecksActor({identity});

      await this.uploadSlides({updateSlides, identity, decksActor});

      await this.deleteSlides({deleteSlides, identity, decksActor});

      // TODO: handle delete decks here?

      await this.clean(syncData);
    } catch (err) {
      syncStore.state.sync = 'error';
      console.error(err);
    }
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
