import {firestore} from 'firebase-admin';

export interface DeployGitHubRepo {
  id: string;
  url: string;
  name: string;
  nameWithOwner: string;
}

export interface DeployGitHub {
  repo?: DeployGitHubRepo;
  status: 'scheduled' | 'failure' | 'successful';
}

export interface DeployApi {
  status: 'scheduled' | 'failure' | 'successful';
}

export interface DeployData {
  owner_id: string;

  github?: DeployGitHub;

  api?: DeployApi;

  updated_at?: firestore.Timestamp;
}

export interface Deploy {
  id: string;
  ref: firestore.DocumentReference;
  data: DeployData;
}
