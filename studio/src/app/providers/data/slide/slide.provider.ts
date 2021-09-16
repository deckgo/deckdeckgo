import {Slide} from '@deckdeckgo/editor';

import {SlideIcProvider} from './slide.ic.provider';
import {SlideOfflineProvider} from './slide.offline.provider';

import {firebase, internetComputer} from '../../../utils/core/environment.utils';

export const getSlide = async (deckId: string, slideId: string): Promise<Slide> => {
  if (internetComputer()) {
    return SlideIcProvider.getInstance().get(deckId, slideId);
  }

  if (firebase()) {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {getSlide: getUserSlide} = await import(cdn);

    return getUserSlide(deckId, slideId);
  }

  return SlideOfflineProvider.getInstance().get(deckId, slideId);
};
