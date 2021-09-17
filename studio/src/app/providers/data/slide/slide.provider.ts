import {Slide, GetSlide} from '@deckdeckgo/editor';

import {SlideIcProvider} from './slide.ic.provider';
import {SlideOfflineProvider} from './slide.offline.provider';

import {firebase, internetComputer} from '../../../utils/core/environment.utils';
import {provider} from '../../../utils/core/providers.utils';

export const getSlide = async (deckId: string, slideId: string): Promise<Slide> => {
  if (internetComputer()) {
    return SlideIcProvider.getInstance().get(deckId, slideId);
  }

  if (firebase()) {
    const {getSlide: getUserSlide}: {getSlide: GetSlide} = await provider<{getSlide: GetSlide}>();

    return getUserSlide(deckId, slideId);
  }

  return SlideOfflineProvider.getInstance().get(deckId, slideId);
};
