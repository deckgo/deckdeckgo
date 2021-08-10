import type {Principal} from '@dfinity/principal';
export interface Attribute {
  value: string;
  name: string;
}
export interface Deck {
  data: DeckData;
  deckId: DeckId__1;
}
export interface DeckBucket {
  del: (arg_0: boolean) => Promise<boolean>;
  get: () => Promise<Deck>;
  set: (arg_0: Deck) => Promise<undefined>;
}
export interface DeckData {
  updated_at: [] | [Time];
  background: [] | [string];
  meta: [] | [DeckMeta];
  name: string;
  created_at: [] | [Time];
  slides: [] | [Array<SlideId>];
  attributes: [] | [Array<Attribute>];
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
export type DeckId__1 = string;
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
export type SlideId = string;
export type Time = bigint;
export interface UserSocial {
  dev: [] | [string];
  linkedin: [] | [string];
  twitter: [] | [string];
  custom_logo_url: [] | [string];
  custom: [] | [string];
  github: [] | [string];
  medium: [] | [string];
}
export interface _SERVICE {
  entries: () => Promise<Array<DeckBucket>>;
  entriesAdmin: (arg_0: Principal) => Promise<Array<DeckBucket>>;
  get: (arg_0: DeckId) => Promise<Principal>;
  init: (arg_0: DeckId) => Promise<Principal>;
}
