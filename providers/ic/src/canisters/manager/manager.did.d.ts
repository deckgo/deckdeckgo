import type {Principal} from '@dfinity/principal';
export interface Bucket {
  owner: UserId;
  bucketId: [] | [BucketId];
}
export type BucketId = Principal;
export type UserId = Principal;
export interface _SERVICE {
  delData: () => Promise<boolean>;
  delStorage: () => Promise<boolean>;
  getData: () => Promise<[] | [Bucket]>;
  getStorage: () => Promise<[] | [Bucket]>;
  initData: () => Promise<Bucket>;
  initStorage: () => Promise<Bucket>;
  installCode: (arg_0: Principal, arg_1: Array<number>, arg_2: Array<number>) => Promise<undefined>;
  list: (arg_0: string) => Promise<Array<Bucket>>;
}
