import {firestore} from 'firebase-admin';

export interface PlatformGitHub {
  token: string;
}

export interface PlatformData {
  github?: PlatformGitHub;

  updated_at?: firestore.Timestamp;
}

export interface Platform {
  id: string;
  data: PlatformData;
  ref: firestore.DocumentReference;
}
