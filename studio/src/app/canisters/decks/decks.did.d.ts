import type {Principal} from '@dfinity/principal';
export type DeckBucketId = Principal;
export type DeckId = string;
export interface _SERVICE {
  entries: () => Promise<Array<DeckBucketId>>;
  entriesAdmin: (arg_0: Principal) => Promise<Array<DeckBucketId>>;
  get: (arg_0: DeckId) => Promise<DeckBucketId>;
  init: (arg_0: DeckId) => Promise<DeckBucketId>;
}
