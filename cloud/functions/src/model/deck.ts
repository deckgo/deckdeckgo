import {firestore} from 'firebase-admin';

export interface DeckMetaAuthor {
    name: string;
    photo_url?: string;
}

export interface DeckMeta {
    title: string;

    description?: string | firestore.FieldValue;
    tags?: string[] | firestore.FieldValue;

    pathname: string;

    author?: DeckMetaAuthor | firestore.FieldValue;

    published: boolean;
    published_at: firestore.Timestamp;

    updated_at: firestore.Timestamp;
}

export interface DeckAttributes {
    style?: string;
}

export interface DeckData {
    name: string;

    attributes?: DeckAttributes;
    background?: string;

    owner_id: string;

    slides?: string[];

    api_id?: string;

    meta?: DeckMeta;

    created_at?: firestore.Timestamp;
    updated_at?: firestore.Timestamp;
}
