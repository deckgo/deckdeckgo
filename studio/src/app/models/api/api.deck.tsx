export interface ApiDeckAttributes {
    style?: string;
}

export interface ApiDeck {
    id?: string;
    slides: string[];
    name: string;
    owner_id: string;
    attributes?: ApiDeckAttributes;
    background?: string;
}
