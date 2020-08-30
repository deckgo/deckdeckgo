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

  updated_at?: firebase.firestore.Timestamp;
  created_at?: firebase.firestore.Timestamp;
}

export interface Deploy {
  id: string;
  data: DeployData | undefined;
}
