export interface DeckMetaAuthor {
    name: string;
    photo_url?: string;
}

export interface DeckMeta {
    title: string;

    description?: string | firebase.firestore.FieldValue;
    tags?: string[] | firebase.firestore.FieldValue;

    pathname: string;

    author?: DeckMetaAuthor | firebase.firestore.FieldValue;

    published: boolean;
    published_at: firebase.firestore.Timestamp;
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

    created_at?: firebase.firestore.Timestamp;
    updated_at?: firebase.firestore.Timestamp;
}

export interface Deck {
    id: string;
    data: DeckData;
}

