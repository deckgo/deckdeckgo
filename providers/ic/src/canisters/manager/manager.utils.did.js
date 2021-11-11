export const idlFactory = ({IDL}) => {
  const BucketId = IDL.Principal;
  return IDL.Service({
    delDeck: IDL.Func([], [IDL.Bool], []),
    delStorage: IDL.Func([], [IDL.Bool], []),
    deleteDeckAdmin: IDL.Func([IDL.Principal], [IDL.Bool], []),
    deleteStorageAdmin: IDL.Func([IDL.Principal], [IDL.Bool], []),
    getDeck: IDL.Func([], [IDL.Opt(BucketId)], ['query']),
    getStorage: IDL.Func([], [IDL.Opt(BucketId)], ['query']),
    initDeck: IDL.Func([], [BucketId], []),
    initStorage: IDL.Func([], [BucketId], [])
  });
};
export const init = ({IDL}) => {
  return [];
};
