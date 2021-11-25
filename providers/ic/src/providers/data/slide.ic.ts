import {Slide, SlideData} from '@deckdeckgo/editor';

import {getData} from '../../utils/data.utils';

export const getSlide = (deckId: string, slideId: string): Promise<Slide | undefined> =>
  getData<Slide, SlideData>({key: `/decks/${deckId}/slides/${slideId}`});
