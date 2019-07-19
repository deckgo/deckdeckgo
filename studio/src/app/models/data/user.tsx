export interface UserSocial {
    twitter?: string;
    linkedin?: string;
    dev?: string;
    medium?: string;
    github?: string;
    custom?: string;
}

export interface UserData {
    anonymous: boolean;

    name?: string;
    email?: string;
    newsletter?: boolean;
    photo_url?: string;

    social?: UserSocial;

    bio?: string;

    created_at: firebase.firestore.Timestamp;
    updated_at: firebase.firestore.Timestamp;
}

export interface User {
    id: string;
    data: UserData;
}

