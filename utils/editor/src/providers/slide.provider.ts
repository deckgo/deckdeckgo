import {Slide} from '../models/data/slide';

export interface GetSlide {
  (deckId: string, slideId: string): Promise<Slide | undefined>;
}

export interface UpdateSlide {
  (deckId: string, slide: Slide): Promise<Slide>;
}

export interface DeleteSlide {
  (deckId: string, slideId: string, updated_at?: Date | number | BigInt): Promise<void>;
}
