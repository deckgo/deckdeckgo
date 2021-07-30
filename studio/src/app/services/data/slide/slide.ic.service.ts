import {Identity} from '@dfinity/agent';

import {Slide} from '../../../models/data/slide';

import {idlFactory as SlideFactory} from '../../../functions/slides/slides.utils.did';
import {_SERVICE as SlideActor} from '../../../functions/slides/slides.did';

import {CanisterUtils} from '../../../utils/editor/canister.utils';

import {createActor} from '../../../utils/core/ic.utils';

export class SlideIcService {
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

    await slideActor.set({
      slideId: slide.id,
      deckId,
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

  async deleteSlide({slideId, slideActor}: {slideId: string; slideActor: SlideActor}) {
    if (!slideId) {
      return;
    }

    await slideActor.del(slideId);
  }
}
