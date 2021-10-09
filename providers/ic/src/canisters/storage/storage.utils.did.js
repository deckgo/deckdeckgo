export const idlFactory = ({IDL}) => {
  const UserId = IDL.Principal;
  const Chunk = IDL.Record({
    content: IDL.Vec(IDL.Nat8),
    batchId: IDL.Nat
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
    transferCycles: IDL.Func([], [], [])
  });
  return StorageBucket;
};
export const init = ({IDL}) => {
  const UserId = IDL.Principal;
  return [UserId];
};
