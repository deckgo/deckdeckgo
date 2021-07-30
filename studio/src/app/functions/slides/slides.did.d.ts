import type {Principal} from '@dfinity/principal';
export interface Slide {
  id: SlideId;
  data: SlideData;
}
export interface SlideAttribute {
  value: string;
  name: string;
}
export interface SlideData {
  updated_at: [] | [Time];
  content: [] | [string];
  created_at: [] | [Time];
  scope: [] | [string];
  attributes: [] | [Array<SlideAttribute>];
  template: string;
}
export type SlideId = string;
export type SlideId__1 = string;
export type Time = bigint;
export interface _SERVICE {
  del: (arg_0: SlideId__1) => Promise<boolean>;
  get: (arg_0: SlideId__1) => Promise<Slide>;
  set: (arg_0: Slide) => Promise<undefined>;
}
