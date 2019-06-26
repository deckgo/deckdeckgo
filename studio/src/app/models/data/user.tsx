export interface UserData {
    anonymous: boolean;

    created_at: firebase.firestore.Timestamp;
    updated_at: firebase.firestore.Timestamp;
}

export interface User {
    id: string;
    data: UserData;
}

