import {GetSlide, Slide} from '@deckdeckgo/editor';
import {getOfflineSlide} from '@deckdeckgo/offline';
import {EnvStore} from '../stores/env.store';
import {cloudProvider} from '../utils/providers.utils';

export const getSlide = async (deckId: string, slideId: string): Promise<Slide> => {
  if (EnvStore.getInstance().cloud()) {
    const {getSlide: getUserSlide}: {getSlide: GetSlide} = await cloudProvider<{getSlide: GetSlide}>();

    return getUserSlide(deckId, slideId);
  }

  return getOfflineSlide(deckId, slideId);
};
