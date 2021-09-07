import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {_SERVICE as ManagerActor} from '../../../canisters/manager/manager.did';
import {_SERVICE as DeckBucketActor, Slide as SlideIc} from '../../../canisters/deck/deck.did';

import {Slide, SlideData} from '../../../models/data/slide';
import {createDeckBucketActor, createManagerActor, initDeckBucket} from '../../../utils/core/ic.deck.utils';

import {CanisterUtils} from '../../../utils/editor/canister.utils';

import {SlideProvider} from './slide.provider';

import {AuthFactoryProvider} from '../../auth/auth.factory.provider';
import {AuthIcProvider} from '../../auth/auth.ic.provider';

export class SlideIcProvider implements SlideProvider {
  private static instance: SlideIcProvider;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!SlideIcProvider.instance) {
      SlideIcProvider.instance = new SlideIcProvider();
    }
    return SlideIcProvider.instance;
  }

  // @Override
  async get(deckId: string, slideId: string): Promise<Slide> {
    return new Promise<Slide>(async (resolve, reject) => {
      const identity: Identity | undefined = (AuthFactoryProvider.getInstance() as AuthIcProvider).getIdentity();

      if (!identity) {
        reject('No internet identity.');
        return;
      }

      try {
        const managerActor: ManagerActor = await createManagerActor({identity});

        const bucket: Principal = await initDeckBucket({managerActor, deckId});

        const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket});

        const slide: SlideIc = await deckBucket.getSlide(slideId);

        const data: SlideData = await CanisterUtils.fromArray<SlideData>(slide.data);

        resolve({
          id: slideId,
          data: {
            ...data,
            created_at: CanisterUtils.fromTimestamp(slide.created_at),
            updated_at: CanisterUtils.fromTimestamp(slide.updated_at)
          }
        });
      } catch (err) {
        reject(err);
      }
    });
  }
}
