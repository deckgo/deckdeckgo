import type {Principal} from '@dfinity/principal';
export interface Deck {
  updated_at: Time;
  data: Array<number>;
  deckId: DeckId;
  created_at: Time;
}
export interface DeckBucket {
  del: () => Promise<boolean>;
  delAdmin: (arg_0: UserId__1) => Promise<boolean>;
  delSlide: (arg_0: SlideId__1) => Promise<boolean>;
  get: () => Promise<Deck>;
  getSlide: (arg_0: SlideId__1) => Promise<Slide>;
  id: () => Promise<Principal>;
  set: (arg_0: Deck) => Promise<undefined>;
  setSlide: (arg_0: Slide) => Promise<undefined>;
}
export type DeckId = string;
export interface Slide {
  updated_at: Time;
  data: Array<number>;
  created_at: Time;
  slideId: SlideId;
}
export type SlideId = string;
export type SlideId__1 = string;
export type Time = bigint;
export type UserId = Principal;
export type UserId__1 = Principal;
export interface _SERVICE extends DeckBucket {}
