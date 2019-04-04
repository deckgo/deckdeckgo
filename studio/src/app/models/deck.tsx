// TODO Change owner in user or user in owner but preserve naming

export interface Deck {
    deck_id?: string;
    deck_slides: string[];
    deck_name: string;
    deck_owner_id: string;
}
