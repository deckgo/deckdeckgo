import type {Principal} from '@dfinity/principal';
export interface AssetKey {
  token: [] | [string];
  name: string;
  fullPath: string;
  folder: string;
}
export interface Chunk {
  content: Array<number>;
  batchId: bigint;
}
export type HeaderField = [string, string];
export type HeaderField__1 = [string, string];
export interface HttpRequest {
  url: string;
  method: string;
  body: Array<number>;
  headers: Array<HeaderField>;
}
export interface HttpResponse {
  body: Array<number>;
  headers: Array<HeaderField>;
  streaming_strategy: [] | [StreamingStrategy];
  status_code: number;
}
export interface StorageBucket {
  commitUpload: (arg_0: {headers: Array<HeaderField__1>; chunkIds: Array<bigint>; batchId: bigint}) => Promise<undefined>;
  del: (arg_0: {token: [] | [string]; fullPath: string}) => Promise<undefined>;
  http_request: (arg_0: HttpRequest) => Promise<HttpResponse>;
  http_request_streaming_callback: (arg_0: StreamingCallbackToken) => Promise<StreamingCallbackHttpResponse>;
  initUpload: (arg_0: AssetKey) => Promise<{batchId: bigint}>;
  list: (arg_0: [] | [string]) => Promise<Array<AssetKey>>;
  transferCycles: () => Promise<undefined>;
  uploadChunk: (arg_0: Chunk) => Promise<{chunkId: bigint}>;
}
export interface StreamingCallbackHttpResponse {
  token: [] | [StreamingCallbackToken__1];
  body: Array<number>;
}
export interface StreamingCallbackToken {
  token: [] | [string];
  sha256: [] | [Array<number>];
  fullPath: string;
  headers: Array<HeaderField>;
  index: bigint;
}
export interface StreamingCallbackToken__1 {
  token: [] | [string];
  sha256: [] | [Array<number>];
  fullPath: string;
  headers: Array<HeaderField>;
  index: bigint;
}
export type StreamingStrategy = {
  Callback: {
    token: StreamingCallbackToken__1;
    callback: [Principal, string];
  };
};
export type UserId = Principal;
export interface _SERVICE extends StorageBucket {}
