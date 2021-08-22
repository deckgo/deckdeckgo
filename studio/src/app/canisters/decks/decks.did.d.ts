import type {Principal} from '@dfinity/principal';
export type DeckBucketId = Principal;
export type DeckId = string;
export interface _SERVICE {
  del: (arg_0: DeckId) => Promise<boolean>;
  deleteDecksAdmin: (arg_0: Principal) => Promise<undefined>;
  entries: () => Promise<Array<DeckBucketId>>;
  get: (arg_0: DeckId) => Promise<DeckBucketId>;
  init: (arg_0: DeckId) => Promise<DeckBucketId>;
}
