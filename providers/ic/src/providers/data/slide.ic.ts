import {Slide, SlideData} from '@deckdeckgo/editor';

import {getData} from '../../utils/data.utils';

export const getSlide = (deckId: string, slideId: string): Promise<Slide> =>
  getData<Slide, SlideData>({key: `/decks/${deckId}/slides/${slideId}`});
