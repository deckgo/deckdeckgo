export interface PlatformDeckGitHubRepo {
  id: string;
  url: string;
  name: string;
  nameWithOwner: string;
}

export interface PlatformDeckGitHub {
  repo: PlatformDeckGitHubRepo;
}

export interface PlatformDeckData {
  github: PlatformDeckGitHub;

  updated_at?: firebase.firestore.Timestamp;
  created_at?: firebase.firestore.Timestamp;
}

export interface PlatformDeck {
  id: string;
  data: PlatformDeckData;
}
