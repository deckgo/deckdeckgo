export interface UserSocial {
  twitter?: string;
  linkedin?: string;
  dev?: string;
  medium?: string;
  github?: string;

  custom?: string;
  custom_logo_url?: string;
}

export interface UserData {
  anonymous: boolean;

  name?: string;
  email?: string;
  newsletter?: boolean;
  photo_url?: string;

  social?: UserSocial;
}
