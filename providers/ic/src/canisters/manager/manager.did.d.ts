import type {Principal} from '@dfinity/principal';
export type BucketId = Principal;
export type DeckId = string;
export interface _SERVICE {
  deckEntries: () => Promise<Array<BucketId>>;
  delDeck: (arg_0: DeckId) => Promise<boolean>;
  delStorage: () => Promise<boolean>;
  deleteDecksAdmin: (arg_0: Principal) => Promise<undefined>;
  deleteStorageAdmin: (arg_0: Principal) => Promise<undefined>;
  getDeck: (arg_0: DeckId) => Promise<[] | [BucketId]>;
  getStorage: () => Promise<[] | [BucketId]>;
  initDeck: (arg_0: DeckId) => Promise<BucketId>;
  initStorage: () => Promise<BucketId>;
}
