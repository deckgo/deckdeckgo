export interface DeckAttributes {
    style?: string;
}

export interface Deck {
    id?: string;
    slides: string[];
    name: string;
    owner_id: string;
    attributes?: DeckAttributes;
    background?: string;
}
