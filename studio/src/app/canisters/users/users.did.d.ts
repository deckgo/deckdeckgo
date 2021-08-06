import type {Principal} from '@dfinity/principal';
export type Time = bigint;
export interface User {
  userId: UserId;
  data: UserData;
}
export interface UserData {
  bio: [] | [string];
  updated_at: [] | [Time];
  photo_url: [] | [string];
  social: [] | [UserSocial];
  name: [] | [string];
  created_at: [] | [Time];
  email: [] | [string];
  newsletter: [] | [boolean];
}
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
export interface _SERVICE {
  del: (arg_0: UserId__1) => Promise<boolean>;
  get: (arg_0: UserId__1) => Promise<User>;
  getUserId: () => Promise<UserId__1>;
  set: (arg_0: User) => Promise<undefined>;
}
