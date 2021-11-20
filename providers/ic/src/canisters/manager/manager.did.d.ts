import type {Principal} from '@dfinity/principal';
export type BucketId = Principal;
export interface _SERVICE {
  delData: () => Promise<boolean>;
  delStorage: () => Promise<boolean>;
  deleteDataAdmin: (arg_0: Principal) => Promise<boolean>;
  deleteStorageAdmin: (arg_0: Principal) => Promise<boolean>;
  getData: () => Promise<[] | [BucketId]>;
  getStorage: () => Promise<[] | [BucketId]>;
  initData: () => Promise<BucketId>;
  initStorage: () => Promise<BucketId>;
  installCodeData: (arg_0: Array<number>) => Promise<undefined>;
  installCodeStorage: (arg_0: Array<number>) => Promise<undefined>;
}
