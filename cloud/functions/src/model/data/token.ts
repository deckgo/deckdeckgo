import {firestore} from 'firebase-admin';

export interface TokenGitHub {
  token: string;
}

export interface TokenData {
  github?: TokenGitHub;

  updated_at?: firestore.Timestamp;
}

export interface Token {
  id: string;
  data: TokenData;
  ref: firestore.DocumentReference;
}
