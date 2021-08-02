import {Deck} from '../../../models/data/deck';

export interface DeckService {
  entries(userId: string): Promise<Deck[]>;
}
