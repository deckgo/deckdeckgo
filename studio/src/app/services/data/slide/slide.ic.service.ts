import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {_SERVICE as DecskActor, _SERVICE as DecksActor} from '../../../canisters/decks/decks.did';
import {_SERVICE as DeckBucketActor, Slide as SlideIc} from '../../../canisters/deck/deck.did';

import {Slide, SlideScope} from '../../../models/data/slide';
import {createDeckBucketActor, createDecksActor} from '../../../utils/core/ic.deck.utils';

import {CanisterUtils} from '../../../utils/editor/canister.utils';

import {SlideService} from './slide.service';
import {AuthFactoryService} from '../../auth/auth.factory.service';
import {AuthIcService} from '../../auth/auth.ic.service';

export class SlideIcService implements SlideService {
  private static instance: SlideIcService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!SlideIcService.instance) {
      SlideIcService.instance = new SlideIcService();
    }
    return SlideIcService.instance;
  }

  async uploadSlide({slide, deckId, decksActor, identity}: {slide: Slide; deckId: string; decksActor: DecksActor; identity: Identity}) {
    if (!slide) {
      return;
    }

    console.log('Slide IC about to SET');
    const t0 = performance.now();

    const bucket: Principal = await decksActor.init(deckId);

    const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket});

    await deckBucket.setSlide({
      slideId: slide.id,
      data: {
        content: CanisterUtils.toNullable<string>(slide.data.content),
        template: slide.data.template,
        scope: CanisterUtils.toNullable<string>(slide.data.scope),
        attributes: CanisterUtils.toAttributes(slide.data.attributes),
        created_at: CanisterUtils.toNullableTimestamp(slide.data.created_at as Date | undefined),
        updated_at: CanisterUtils.toNullableTimestamp(slide.data.updated_at as Date | undefined)
      }
    });

    const t1 = performance.now();
    console.log('Slide IC SET done', t1 - t0);

    const t2 = performance.now();

    // TODO: remove, just for test
    console.log('Slide IC Get:', await deckBucket.getSlide(slide.id), performance.now() - t2);
  }

  async deleteSlide({slideId, deckId, decksActor, identity}: {slideId: string; deckId: string; decksActor: DecksActor; identity: Identity}) {
    if (!slideId) {
      return;
    }

    console.log('Slide IC about to DEL');
    const t0 = performance.now();

    const bucket: Principal = await decksActor.init(deckId);

    const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket});

    await deckBucket.delSlide(slideId);

    const t1 = performance.now();
    console.log('Slide IC DEL done', t1 - t0);
  }

  // @Override
  async get(deckId: string, slideId: string): Promise<Slide> {
    return new Promise<Slide>(async (resolve, reject) => {
      const identity: Identity | undefined = (AuthFactoryService.getInstance() as AuthIcService).getIdentity();

      if (!identity) {
        reject('No internet identity.');
        return;
      }

      try {
        const decksActor: DecskActor = await createDecksActor({identity});

        const bucket: Principal = await decksActor.init(deckId);

        const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket});

        const slide: SlideIc = await deckBucket.getSlide(slideId);

        resolve({
          id: slideId,
          data: {
            content: CanisterUtils.fromNullable<string>(slide.data.content),
            template: slide.data.template,
            scope: CanisterUtils.fromNullable<SlideScope>(slide.data.scope as [] | [SlideScope]),
            attributes: CanisterUtils.fromAttributes(slide.data.attributes),
            created_at: CanisterUtils.fromNullableTimestamp(slide.data.created_at),
            updated_at: CanisterUtils.fromNullableTimestamp(slide.data.updated_at)
          }
        });
      } catch (err) {
        reject(err);
      }
    });
  }
}
