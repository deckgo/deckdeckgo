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
  installCode: (arg_0: Array<number>, arg_1: string) => Promise<undefined>;
}
