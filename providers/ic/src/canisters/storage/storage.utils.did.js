export const idlFactory = ({IDL}) => {
  const UserId = IDL.Principal;
  const StorageBucket = IDL.Service({
    create_batch: IDL.Func([IDL.Record({token: IDL.Text})], [IDL.Record({batchId: IDL.Nat})], []),
    transferCycles: IDL.Func([], [], [])
  });
  return StorageBucket;
};
export const init = ({IDL}) => {
  const UserId = IDL.Principal;
  return [UserId];
};
