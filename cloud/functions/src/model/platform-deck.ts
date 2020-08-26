import {firestore} from 'firebase-admin';

export interface GitHubRepo {
  id: string;
  url: string;
  name: string;
  nameWithOwner: string;
}

export interface GitHub {
  repo: GitHubRepo;
}

export interface PlatformDeckData {
  github: GitHub;

  created_at?: firestore.Timestamp;
  updated_at?: firestore.Timestamp;
}

export interface PlatformDeck {
  id: string;
  ref: firestore.DocumentReference;
  data: PlatformDeckData;
}
