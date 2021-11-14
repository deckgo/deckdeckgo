export interface AuthUser {
  state: 'initialization' | 'authenticated';

  uid?: string;

  gitHub?: boolean;

  name?: string;
  email?: string;
  email_verified?: boolean;
  photo_url?: string;
}
