import type {UserSocial} from './user';

export interface Author {
  name: string;
  bio?: string;
  photo_url?: string;
  social?: UserSocial;
}

export interface Meta {
  title: string;

  description?: string;
  tags?: string[];

  pathname?: string;

  canonical?: string;

  author?: Author;

  published?: boolean;
  published_at?: Date | number | BigInt;

  feed?: boolean;

  updated_at: Date | number | BigInt;
}
