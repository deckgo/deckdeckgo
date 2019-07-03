export interface UserData {
    anonymous: boolean;

    name?: string;
    email?: string;
    newsletter?: boolean;
    photo_url?: string;

    created_at: firebase.firestore.Timestamp;
    updated_at: firebase.firestore.Timestamp;
}

export interface User {
    id: string;
    data: UserData;
}

