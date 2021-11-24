export const idlFactory = ({IDL}) => {
  const BucketId = IDL.Principal;
  const UserId = IDL.Principal;
  return IDL.Service({
    delData: IDL.Func([], [IDL.Bool], []),
    delStorage: IDL.Func([], [IDL.Bool], []),
    deleteDataAdmin: IDL.Func([IDL.Principal], [IDL.Bool], []),
    deleteStorageAdmin: IDL.Func([IDL.Principal], [IDL.Bool], []),
    getData: IDL.Func([], [IDL.Opt(BucketId)], ['query']),
    getStorage: IDL.Func([], [IDL.Opt(BucketId)], ['query']),
    initData: IDL.Func([], [BucketId], []),
    initStorage: IDL.Func([], [BucketId], []),
    installCode: IDL.Func([IDL.Principal, UserId, IDL.Vec(IDL.Nat8)], [], []),
    list: IDL.Func([IDL.Text], [IDL.Vec(IDL.Record({owner: UserId, bucketId: BucketId}))], ['query'])
  });
};
export const init = ({IDL}) => {
  return [];
};
