// The user information we use to authenticate the user with our backend
export interface ApiUserInfo {
  anonymous: boolean;
  firebase_uid: string;
  email: string;
}

// The representation of the user saved in our database
export interface ApiUser {
  id: string;
  anonymous: boolean;
  firebase_uid: string;
  username?: string;
}
