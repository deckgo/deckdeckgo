export const idlFactory = ({IDL}) => {
  const UserId = IDL.Principal;
  const BucketId = IDL.Principal;
  const Bucket = IDL.Record({
    owner: UserId,
    bucketId: IDL.Opt(BucketId)
  });
  return IDL.Service({
    delData: IDL.Func([], [IDL.Bool], []),
    delStorage: IDL.Func([], [IDL.Bool], []),
    getData: IDL.Func([], [IDL.Opt(Bucket)], ['query']),
    getStorage: IDL.Func([], [IDL.Opt(Bucket)], ['query']),
    initData: IDL.Func([], [Bucket], []),
    initStorage: IDL.Func([], [Bucket], []),
    installCode: IDL.Func([IDL.Text, IDL.Principal, IDL.Vec(IDL.Nat8), IDL.Vec(IDL.Nat8)], [], []),
    list: IDL.Func([IDL.Text, IDL.Text], [IDL.Vec(Bucket)], ['query'])
  });
};
export const init = ({IDL}) => {
  return [];
};
