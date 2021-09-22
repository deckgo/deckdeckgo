import {Slide, GetSlide} from '@deckdeckgo/editor';

import {SlideOfflineProvider} from './slide.offline.provider';

import {cloud} from '../../../utils/core/environment.utils';
import {cloudProvider} from '../../../utils/core/providers.utils';

export const getSlide = async (deckId: string, slideId: string): Promise<Slide> => {
  if (cloud()) {
    const {getSlide: getUserSlide}: {getSlide: GetSlide} = await cloudProvider<{getSlide: GetSlide}>();

    return getUserSlide(deckId, slideId);
  }

  return SlideOfflineProvider.getInstance().get(deckId, slideId);
};
