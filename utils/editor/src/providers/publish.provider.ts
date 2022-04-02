import {Deck} from '../models/data/deck';
import {Doc} from '../models/data/doc';

export interface DeckPublish {
  ({deck, config}: {deck: Deck; config: Record<string, string>}): Promise<Deck>;
}

export interface DocPublish {
  ({doc, config}: {doc: Doc; config: Record<string, string>}): Promise<Doc>;
}

export interface PublishUrl {
  (): Promise<string>;
}
