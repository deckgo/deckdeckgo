import type {DataRecord} from './data';
import type {Deploy} from './deploy';
import type {Meta} from './meta';

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

  meta?: Meta;

  deploy?: Deploy;

  github?: DeckGitHub;

  created_at?: Date | number | BigInt;
  updated_at?: Date | number | BigInt;
}

export type Deck = DataRecord<DeckData>;
