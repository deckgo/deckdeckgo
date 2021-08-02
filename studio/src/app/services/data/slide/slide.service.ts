import {Slide} from '../../../models/data/slide';

export interface SlideService {
  get(deckId: string, slideId: string): Promise<Slide>;
}
