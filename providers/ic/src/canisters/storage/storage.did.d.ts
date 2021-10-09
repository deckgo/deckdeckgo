import type {Principal} from '@dfinity/principal';
export interface Chunk {
  content: Array<number>;
  batchId: bigint;
}
export interface StorageBucket {
  commit_batch: (arg_0: {contentType: string; chunkIds: Array<bigint>; batchId: bigint}) => Promise<undefined>;
  create_batch: (arg_0: {token: string; path: string}) => Promise<{batchId: bigint}>;
  create_chunk: (arg_0: Chunk) => Promise<{chunkId: bigint}>;
  transferCycles: () => Promise<undefined>;
}
export type UserId = Principal;
export interface _SERVICE extends StorageBucket {}
