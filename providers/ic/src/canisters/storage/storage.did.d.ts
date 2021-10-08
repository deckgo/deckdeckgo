import type {Principal} from '@dfinity/principal';
export interface StorageBucket {
  create_batch: (arg_0: {token: string}) => Promise<{batchId: bigint}>;
  transferCycles: () => Promise<undefined>;
}
export type UserId = Principal;
export interface _SERVICE extends StorageBucket {}
