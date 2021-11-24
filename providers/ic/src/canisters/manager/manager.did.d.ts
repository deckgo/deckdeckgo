import type {Principal} from '@dfinity/principal';
export type BucketId = Principal;
export type UserId = Principal;
export interface _SERVICE {
  delData: () => Promise<boolean>;
  delStorage: () => Promise<boolean>;
  deleteDataAdmin: (arg_0: Principal) => Promise<boolean>;
  deleteStorageAdmin: (arg_0: Principal) => Promise<boolean>;
  getData: () => Promise<[] | [BucketId]>;
  getStorage: () => Promise<[] | [BucketId]>;
  initData: () => Promise<BucketId>;
  initStorage: () => Promise<BucketId>;
  installCode: (arg_0: Principal, arg_1: UserId, arg_2: Array<number>) => Promise<undefined>;
  list: (arg_0: string) => Promise<Array<{owner: UserId; bucketId: BucketId}>>;
}
