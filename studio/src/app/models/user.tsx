export interface User {
    token: string;

    anonymous: boolean;

    name?: string;
    email?: string;
    email_verified?: boolean;
    photo_url?: string;
}
