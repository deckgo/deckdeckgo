export const idlFactory = ({IDL}) => {
  const UserId = IDL.Principal;
  const HeaderField__1 = IDL.Tuple(IDL.Text, IDL.Text);
  const HeaderField = IDL.Tuple(IDL.Text, IDL.Text);
  const HttpRequest = IDL.Record({
    url: IDL.Text,
    method: IDL.Text,
    body: IDL.Vec(IDL.Nat8),
    headers: IDL.Vec(HeaderField)
  });
  const StreamingCallbackToken__1 = IDL.Record({
    token: IDL.Opt(IDL.Text),
    sha256: IDL.Opt(IDL.Vec(IDL.Nat8)),
    fullPath: IDL.Text,
    headers: IDL.Vec(HeaderField),
    index: IDL.Nat
  });
  const StreamingStrategy = IDL.Variant({
    Callback: IDL.Record({
      token: StreamingCallbackToken__1,
      callback: IDL.Func([], [], [])
    })
  });
  const HttpResponse = IDL.Record({
    body: IDL.Vec(IDL.Nat8),
    headers: IDL.Vec(HeaderField),
    streaming_strategy: IDL.Opt(StreamingStrategy),
    status_code: IDL.Nat16
  });
  const StreamingCallbackToken = IDL.Record({
    token: IDL.Opt(IDL.Text),
    sha256: IDL.Opt(IDL.Vec(IDL.Nat8)),
    fullPath: IDL.Text,
    headers: IDL.Vec(HeaderField),
    index: IDL.Nat
  });
  const StreamingCallbackHttpResponse = IDL.Record({
    token: IDL.Opt(StreamingCallbackToken__1),
    body: IDL.Vec(IDL.Nat8)
  });
  const AssetKey = IDL.Record({
    token: IDL.Opt(IDL.Text),
    name: IDL.Text,
    fullPath: IDL.Text,
    folder: IDL.Text
  });
  const Chunk = IDL.Record({
    content: IDL.Vec(IDL.Nat8),
    batchId: IDL.Nat
  });
  const StorageBucket = IDL.Service({
    commitUpload: IDL.Func(
      [
        IDL.Record({
          headers: IDL.Vec(HeaderField__1),
          chunkIds: IDL.Vec(IDL.Nat),
          batchId: IDL.Nat
        })
      ],
      [],
      []
    ),
    del: IDL.Func([IDL.Record({token: IDL.Opt(IDL.Text), fullPath: IDL.Text})], [], []),
    http_request: IDL.Func([HttpRequest], [HttpResponse], ['query']),
    http_request_streaming_callback: IDL.Func([StreamingCallbackToken], [StreamingCallbackHttpResponse], ['query']),
    initUpload: IDL.Func([AssetKey], [IDL.Record({batchId: IDL.Nat})], []),
    list: IDL.Func([IDL.Opt(IDL.Text)], [IDL.Vec(AssetKey)], ['query']),
    transferCycles: IDL.Func([], [], []),
    uploadChunk: IDL.Func([Chunk], [IDL.Record({chunkId: IDL.Nat})], [])
  });
  return StorageBucket;
};
export const init = ({IDL}) => {
  const UserId = IDL.Principal;
  return [UserId];
};
