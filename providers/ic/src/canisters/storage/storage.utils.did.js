export const idlFactory = ({IDL}) => {
  const UserId = IDL.Principal;
  const Chunk = IDL.Record({
    content: IDL.Vec(IDL.Nat8),
    batchId: IDL.Nat
  });
  const HeaderField = IDL.Tuple(IDL.Text, IDL.Text);
  const HttpRequest = IDL.Record({
    url: IDL.Text,
    method: IDL.Text,
    body: IDL.Vec(IDL.Nat8),
    headers: IDL.Vec(HeaderField)
  });
  const StreamingCallbackToken = IDL.Record({
    key: IDL.Text,
    index: IDL.Nat,
    content_encoding: IDL.Text
  });
  const StreamingStrategy = IDL.Variant({
    Callback: IDL.Record({
      token: StreamingCallbackToken,
      callback: IDL.Func([], [], [])
    })
  });
  const HttpResponse = IDL.Record({
    body: IDL.Vec(IDL.Nat8),
    headers: IDL.Vec(HeaderField),
    streaming_strategy: IDL.Opt(StreamingStrategy),
    status_code: IDL.Nat16
  });
  const StorageBucket = IDL.Service({
    commit_batch: IDL.Func(
      [
        IDL.Record({
          contentType: IDL.Text,
          chunkIds: IDL.Vec(IDL.Nat),
          batchId: IDL.Nat
        })
      ],
      [],
      []
    ),
    create_batch: IDL.Func([IDL.Record({token: IDL.Text, path: IDL.Text})], [IDL.Record({batchId: IDL.Nat})], []),
    create_chunk: IDL.Func([Chunk], [IDL.Record({chunkId: IDL.Nat})], []),
    http_request: IDL.Func([HttpRequest], [HttpResponse], ['query']),
    transferCycles: IDL.Func([], [], [])
  });
  return StorageBucket;
};
export const init = ({IDL}) => {
  const UserId = IDL.Principal;
  return [UserId];
};
