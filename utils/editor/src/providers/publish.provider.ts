import {Deck} from '../models/data/deck';

export interface Publish {
  ({deck: deckSource, config}: {deck: Deck; config: Record<string, string>}): Promise<Deck>;
}
