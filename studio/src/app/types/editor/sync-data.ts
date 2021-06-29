import { Deck } from '../../models/data/deck';
import { Slide } from '../../models/data/slide';

export interface SyncData {
  deckId: string;
  deck: Deck | undefined;
  slides: Slide[] | undefined;
}
