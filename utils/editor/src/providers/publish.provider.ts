import {Deck} from '../models/data/deck';
import {Doc} from '../models/data/doc';

export interface DeckPublish {
  ({deck, config}: {deck: Deck; config: Record<string, string | boolean>}): Promise<Deck>;
}

export interface DocPublish {
  ({doc, config}: {doc: Doc; config: Record<string, string | boolean>}): Promise<Doc>;
}

export interface PublishUrl {
  (): Promise<string>;
}

export interface UpdateLanding {
  (): Promise<void>;
}
