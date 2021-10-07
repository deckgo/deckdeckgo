import type {Principal} from '@dfinity/principal';
export type BucketId = Principal;
export type DeckId = string;
export interface _SERVICE {
  deckEntries: () => Promise<Array<BucketId>>;
  delDeck: (arg_0: DeckId) => Promise<boolean>;
  deleteDecksAdmin: (arg_0: Principal) => Promise<undefined>;
  getDeck: (arg_0: DeckId) => Promise<[] | [BucketId]>;
  initDeck: (arg_0: DeckId) => Promise<BucketId>;
}
