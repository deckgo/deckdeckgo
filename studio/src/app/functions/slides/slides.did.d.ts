import type {Principal} from '@dfinity/principal';
export interface Slide {
  id: SlideId;
  data: SlideData;
}
export interface SlideData {
  content: [] | [string];
}
export type SlideId = string;
export type SlideId__1 = string;
export interface _SERVICE {
  del: (arg_0: SlideId__1) => Promise<undefined>;
  get: (arg_0: SlideId__1) => Promise<Slide>;
  set: (arg_0: Slide) => Promise<undefined>;
}
