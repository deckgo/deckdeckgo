import type {DataRecord} from './data';

export interface UserSocial {
  twitter?: string;
  linkedin?: string;
  dev?: string;
  medium?: string;
  github?: string;

  custom?: string;
  custom_logo_url?: string;
}

export interface UserData {
  name?: string;
  email?: string;
  newsletter?: boolean;
  photo_url?: string;

  social?: UserSocial;

  bio?: string;

  // Except Firebase for which we handle username in the api, see ApiUser
  username?: string;

  created_at: Date | number | BigInt;
  updated_at: Date | number | BigInt;
}

export type User = DataRecord<UserData>;
