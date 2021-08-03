import type {Principal} from '@dfinity/principal';
export interface Attribute {
  value: string;
  name: string;
}
export interface Deck {
  data: DeckData;
  deckId: DeckId;
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
  del: (arg_0: DeckId__1, arg_1: boolean) => Promise<boolean>;
  entries: () => Promise<Array<Deck>>;
  get: (arg_0: DeckId__1) => Promise<Deck>;
  set: (arg_0: Deck) => Promise<undefined>;
}
