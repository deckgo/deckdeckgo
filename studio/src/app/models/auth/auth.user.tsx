export interface AuthUser {
  uid: string;

  anonymous: boolean;

  gitHub: boolean;

  name?: string;
  email?: string;
  email_verified?: boolean;
  photo_url?: string;
}
