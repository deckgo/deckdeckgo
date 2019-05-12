// The user information we use to authenticate the user with our backend
export interface UserInfo {
    anonymous: boolean;
    firebase_uid: string;
    email: string;
}

// The representation of the user saved in our database
export interface User {
    id: string;
    anonymous: boolean;
    username?: string;
}
