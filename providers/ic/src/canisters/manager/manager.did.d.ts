import type {Principal} from '@dfinity/principal';
export type BucketId = Principal;
export type DeckId = string;
export interface _SERVICE {
  del: (arg_0: DeckId) => Promise<boolean>;
  deleteDecksAdmin: (arg_0: Principal) => Promise<undefined>;
  entries: () => Promise<Array<BucketId>>;
  get: (arg_0: DeckId) => Promise<[] | [BucketId]>;
  init: (arg_0: DeckId) => Promise<BucketId>;
}
