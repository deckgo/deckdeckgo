export interface UserData {
    anonymous: boolean;

    name?: string;
    photo_url?: string;

    created_at: firebase.firestore.Timestamp;
    updated_at: firebase.firestore.Timestamp;
}

export interface User {
    id: string;
    data: UserData;
}

