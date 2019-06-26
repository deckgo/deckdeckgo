import {DeckAttributes} from '../data/deck';

export interface ApiDeck {
    id?: string;
    slides: string[];
    name: string;
    owner_id: string;
    attributes?: DeckAttributes;
    background?: string;
}
