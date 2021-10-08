export const idlFactory = ({IDL}) => {
  const UserId = IDL.Principal;
  const Chunk = IDL.Record({
    content: IDL.Vec(IDL.Nat8),
    batchId: IDL.Nat
  });
  const StorageBucket = IDL.Service({
    create_batch: IDL.Func([IDL.Record({token: IDL.Text})], [IDL.Record({batchId: IDL.Nat})], []),
    create_chunk: IDL.Func([Chunk], [IDL.Record({chunkId: IDL.Nat})], []),
    transferCycles: IDL.Func([], [], [])
  });
  return StorageBucket;
};
export const init = ({IDL}) => {
  const UserId = IDL.Principal;
  return [UserId];
};
