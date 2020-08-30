export interface TokenGitHub {
  token: string;
}

export interface TokenData {
  github?: TokenGitHub;

  updated_at?: firebase.firestore.Timestamp;
}

export interface Token {
  id: string;
  data: TokenData;
}
