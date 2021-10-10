import type {Principal} from '@dfinity/principal';
export interface Chunk {
  content: Array<number>;
  batchId: bigint;
}
export type HeaderField = [string, string];
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
  commit_batch: (arg_0: {contentType: string; chunkIds: Array<bigint>; batchId: bigint}) => Promise<undefined>;
  create_batch: (arg_0: {token: string; path: string}) => Promise<{batchId: bigint}>;
  create_chunk: (arg_0: Chunk) => Promise<{chunkId: bigint}>;
  http_request: (arg_0: HttpRequest) => Promise<HttpResponse>;
  http_request_streaming_callback: (arg_0: StreamingCallbackToken) => Promise<StreamingCallbackHttpResponse>;
  transferCycles: () => Promise<undefined>;
}
export interface StreamingCallbackHttpResponse {
  token: [] | [StreamingCallbackToken__1];
  body: Array<number>;
}
export interface StreamingCallbackToken {
  token: string;
  path: string;
  index: bigint;
  contentEncoding: string;
}
export interface StreamingCallbackToken__1 {
  token: string;
  path: string;
  index: bigint;
  contentEncoding: string;
}
export type StreamingStrategy = {
  Callback: {
    token: StreamingCallbackToken__1;
    callback: [Principal, string];
  };
};
export type UserId = Principal;
export interface _SERVICE extends StorageBucket {}
