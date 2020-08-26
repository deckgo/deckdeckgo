export interface PlatformGitHub {
  token: string;
}

export interface PlatformData {
  github?: PlatformGitHub;

  updated_at?: firebase.firestore.Timestamp;
}

export interface Platform {
  id: string;
  data: PlatformData;
}
