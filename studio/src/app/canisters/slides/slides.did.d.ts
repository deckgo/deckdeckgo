import type {Principal} from '@dfinity/principal';
export interface Attribute {
  value: string;
  name: string;
}
export type DeckId = string;
export interface Slide {
  data: SlideData;
  deckId: DeckId;
  slideId: SlideId;
}
export interface SlideData {
  updated_at: [] | [Time];
  content: [] | [string];
  created_at: [] | [Time];
  scope: [] | [string];
  attributes: [] | [Array<Attribute>];
  template: string;
}
export type SlideId = string;
export type SlideId__1 = string;
export type Time = bigint;
export interface _SERVICE {
  del: (arg_0: SlideId__1) => Promise<boolean>;
  deleteSlide: (arg_0: Principal, arg_1: SlideId__1) => Promise<boolean>;
  get: (arg_0: SlideId__1) => Promise<Slide>;
  set: (arg_0: Slide) => Promise<undefined>;
}
