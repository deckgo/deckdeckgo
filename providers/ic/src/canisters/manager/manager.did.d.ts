import type {Principal} from '@dfinity/principal';
export type BucketId = Principal;
export interface _SERVICE {
  delDeck: () => Promise<boolean>;
  delStorage: () => Promise<boolean>;
  deleteDeckAdmin: (arg_0: Principal) => Promise<boolean>;
  deleteStorageAdmin: (arg_0: Principal) => Promise<boolean>;
  getDeck: () => Promise<[] | [BucketId]>;
  getStorage: () => Promise<[] | [BucketId]>;
  initDeck: () => Promise<BucketId>;
  initStorage: () => Promise<BucketId>;
}
