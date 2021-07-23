import type {Principal} from '@dfinity/principal';
export interface Deck {
  id: DeckId;
  data: DeckData;
}
export interface DeckData {
  name: string;
  header: [] | [string];
}
export type DeckId = string;
export interface _SERVICE {
  del: (arg_0: DeckId) => Promise<boolean>;
  get: (arg_0: DeckId) => Promise<Deck>;
  set: (arg_0: Deck) => Promise<undefined>;
}
