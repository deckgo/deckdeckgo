export interface AuthUser {
    uid: string;
    token: string;

    anonymous: boolean;

    name?: string;
    email?: string;
    email_verified?: boolean;
    photo_url?: string;

    // TODO: More fields
    // twitter url
    // github url
    // linkedin url
    // description

    // following
    // followers

    // created_at
    // updated_at
}
