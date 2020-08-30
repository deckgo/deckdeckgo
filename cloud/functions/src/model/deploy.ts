import {firestore} from 'firebase-admin';

export interface DeployGitHubRepo {
  id: string;
  url: string;
  name: string;
  nameWithOwner: string;
}

export interface DeployGitHub {
  repo: DeployGitHubRepo;
}

export interface DeployData {
  owner_id: string;

  github: DeployGitHub;

  created_at?: firestore.Timestamp;
  updated_at?: firestore.Timestamp;
}

export interface Deploy {
  id: string;
  ref: firestore.DocumentReference;
  data: DeployData;
}
