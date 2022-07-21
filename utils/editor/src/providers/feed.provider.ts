import {Deck} from '../models/data/deck';
import {Doc} from '../models/data/doc';

export interface DeckSubmitFeed {
  ({deck}: {deck: Deck}): Promise<Deck>;
}

export interface DocSubmitFeed {
  ({doc}: {doc: Doc}): Promise<Doc>;
}
