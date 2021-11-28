import {UserSocial} from './user';

export interface DeckDeployData {
  status: 'scheduled' | 'failure' | 'successful';
  updated_at: Date | number | BigInt;
}

export interface DeckDeploy {
  github?: DeckDeployData;
  api?: DeckDeployData;
}

export interface DeckGitHubRepo {
  id: string;
  url: string;
  name: string;
  nameWithOwner: string;
}

export interface DeckGitHub {
  repo?: DeckGitHubRepo;
  publish: boolean;
}

export interface DeckMetaAuthor {
  name: string;
  bio?: string;
  photo_url?: string;
  social?: UserSocial;
}

export interface DeckMeta {
  title: string;

  description?: string;
  tags?: string[];

  pathname?: string;

  author?: DeckMetaAuthor;

  published?: boolean;
  published_at?: Date | number | BigInt;

  feed?: boolean;

  updated_at: Date | number | BigInt;
}

export interface DeckAttributes {
  style?: string;
  animation?: 'slide' | 'fade' | 'none';
  direction?: 'horizontal' | 'vertical' | 'papyrus';
  directionMobile?: 'horizontal' | 'vertical' | 'papyrus';
  autoSlide?: boolean;
}

export interface DeckData {
  name: string;

  attributes?: DeckAttributes;
  background?: string;
  header?: string;
  footer?: string;

  owner_id: string;

  slides?: string[];

  api_id?: string;

  meta?: DeckMeta;

  deploy?: DeckDeploy;

  github?: DeckGitHub;

  created_at?: Date | number | BigInt;
  updated_at?: Date | number | BigInt;
}

export interface Deck {
  id: string;
  data: DeckData;
}
