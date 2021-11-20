export const idlFactory = ({IDL}) => {
  const BucketId = IDL.Principal;
  return IDL.Service({
    delData: IDL.Func([], [IDL.Bool], []),
    delStorage: IDL.Func([], [IDL.Bool], []),
    deleteDataAdmin: IDL.Func([IDL.Principal], [IDL.Bool], []),
    deleteStorageAdmin: IDL.Func([IDL.Principal], [IDL.Bool], []),
    getData: IDL.Func([], [IDL.Opt(BucketId)], ['query']),
    getStorage: IDL.Func([], [IDL.Opt(BucketId)], ['query']),
    initData: IDL.Func([], [BucketId], []),
    initStorage: IDL.Func([], [BucketId], []),
    installCodeData: IDL.Func([IDL.Vec(IDL.Nat8)], [], []),
    installCodeStorage: IDL.Func([IDL.Vec(IDL.Nat8)], [], [])
  });
};
export const init = ({IDL}) => {
  return [];
};
