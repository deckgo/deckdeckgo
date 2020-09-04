import {UserSocial} from './user';

export interface DeckMetaAuthor {
  name: string;
  photo_url?: string;
  social?: UserSocial | firebase.firestore.FieldValue;
}

export interface DeckMeta {
  title: string;

  description?: string | firebase.firestore.FieldValue;
  tags?: string[] | firebase.firestore.FieldValue;

  pathname?: string;

  author?: DeckMetaAuthor | firebase.firestore.FieldValue;

  published?: boolean;
  published_at?: firebase.firestore.Timestamp;

  feed?: boolean;
  github?: boolean;

  updated_at: firebase.firestore.Timestamp;
}

export interface DeckAttributes {
  style?: string;
  transition?: 'slide' | 'fade' | 'none';
}

export interface DeckClone {
  deck_id_from?: string;
  deck_id_to?: string;
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

  clone?: DeckClone;

  created_at?: firebase.firestore.Timestamp;
  updated_at?: firebase.firestore.Timestamp;
}

export interface Deck {
  id: string;
  data: DeckData;
}
