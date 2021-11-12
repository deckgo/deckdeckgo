import type {Principal} from '@dfinity/principal';
export interface Data {
  id: string;
  updated_at: Time;
  data: Array<number>;
  created_at: Time;
}
export interface DataBucket {
  del: (arg_0: string) => Promise<undefined>;
  get: (arg_0: string) => Promise<Data>;
  set: (arg_0: string, arg_1: Data) => Promise<undefined>;
  transferCycles: () => Promise<undefined>;
}
export type Time = bigint;
export type UserId = Principal;
export interface _SERVICE extends DataBucket {}
