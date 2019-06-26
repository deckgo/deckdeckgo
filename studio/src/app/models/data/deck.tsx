export interface DeckAttributes {
    style?: string;
}

export interface DeckData {
    name: string;
    owner_id: string;
    attributes?: DeckAttributes;
    background?: string;

    created_at?: firebase.firestore.Timestamp;
    updated_at?: firebase.firestore.Timestamp;
}

export interface Deck {
    id: string;
    data: DeckData;
}

