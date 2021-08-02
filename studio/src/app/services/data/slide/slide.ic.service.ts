import {Identity} from '@dfinity/agent';

import {Slide, SlideScope} from '../../../models/data/slide';

import {idlFactory as SlideFactory} from '../../../functions/slides/slides.utils.did';
import {_SERVICE as SlideActor, Slide as SlideIc} from '../../../functions/slides/slides.did';

import {CanisterUtils} from '../../../utils/editor/canister.utils';
import {createActor} from '../../../utils/core/ic.utils';

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

  createActor({identity}: {identity: Identity}): Promise<SlideActor> {
    return createActor<SlideActor>({canisterId: process.env.SLIDES_CANISTER_ID, idlFactory: SlideFactory, identity});
  }

  async uploadSlide({slide, deckId, slideActor}: {slide: Slide; deckId: string; slideActor: SlideActor}) {
    if (!slide) {
      return;
    }

    console.log('Slide IC about to SET');

    await slideActor.set({
      slideId: slide.id,
      deckId,
      data: {
        content: CanisterUtils.toNullable<string>(slide.data.content),
        template: slide.data.template,
        scope: CanisterUtils.toNullable<string>(slide.data.scope),
        attributes: CanisterUtils.toAttributes(slide.data.attributes),
        created_at: CanisterUtils.toNullableTimestamp(slide.data.created_at as Date | undefined),
        updated_at: CanisterUtils.toNullableTimestamp(slide.data.updated_at as Date | undefined)
      }
    });

    // TODO: remove, just for test
    console.log('Slide IC Get:', await slideActor.get(slide.id));
  }

  async deleteSlide({slideId, slideActor}: {slideId: string; slideActor: SlideActor}) {
    if (!slideId) {
      return;
    }

    await slideActor.del(slideId);
  }

  // @Override
  get(_deckId: string, slideId: string): Promise<Slide> {
    return new Promise<Slide>(async (resolve, reject) => {
      const identity: Identity | undefined = (AuthFactoryService.getInstance() as AuthIcService).getIdentity();

      if (!identity) {
        reject('No internet identity.');
        return;
      }

      const slideActor: SlideActor = await this.createActor({identity});

      const slide: SlideIc = await slideActor.get(slideId);

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
    });
  }
}
