import type {Principal} from '@dfinity/principal';
export interface Attribute {
  value: string;
  name: string;
}
export interface Attribute__1 {
  value: string;
  name: string;
}
export interface Deck {
  data: DeckData;
  deckId: DeckId;
}
export interface DeckBucket {
  del: () => Promise<boolean>;
  delAdmin: (arg_0: UserId__1) => Promise<boolean>;
  delSlide: (arg_0: SlideId__2) => Promise<boolean>;
  get: () => Promise<Deck>;
  getSlide: (arg_0: SlideId__2) => Promise<Slide>;
  id: () => Promise<Principal>;
  set: (arg_0: Deck) => Promise<undefined>;
  setSlide: (arg_0: Slide) => Promise<undefined>;
}
export interface DeckData {
  updated_at: [] | [Time];
  background: [] | [string];
  meta: [] | [DeckMeta];
  name: string;
  created_at: [] | [Time];
  slides: [] | [Array<SlideId__1>];
  attributes: [] | [Array<Attribute__1>];
  github: [] | [DeckGitHub];
  footer: [] | [string];
  header: [] | [string];
}
export interface DeckGitHub {
  repo: [] | [DeckGitHubRepo];
  publish: boolean;
}
export interface DeckGitHubRepo {
  id: string;
  url: string;
  name: string;
  nameWithOwner: string;
}
export type DeckId = string;
export interface DeckMeta {
  title: string;
  updated_at: Time;
  feed: [] | [boolean];
  published: [] | [boolean];
  tags: [] | [Array<string>];
  pathname: [] | [string];
  description: [] | [string];
  published_at: [] | [Time];
  author: [] | [DeckMetaAuthor];
}
export interface DeckMetaAuthor {
  photo_url: [] | [string];
  social: [] | [UserSocial];
  name: string;
}
export interface Slide {
  data: SlideData;
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
export type SlideId__2 = string;
export type Time = bigint;
export type UserId = Principal;
export type UserId__1 = Principal;
export interface UserSocial {
  dev: [] | [string];
  linkedin: [] | [string];
  twitter: [] | [string];
  custom_logo_url: [] | [string];
  custom: [] | [string];
  github: [] | [string];
  medium: [] | [string];
}
export interface _SERVICE extends DeckBucket {}
