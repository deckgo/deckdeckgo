export interface TokenGitHub {
  token: string;
}

export interface TokenData {
  github?: TokenGitHub;

  updated_at?: Date | number | BigInt;
}

export interface Token {
  id: string;
  data: TokenData;
}
