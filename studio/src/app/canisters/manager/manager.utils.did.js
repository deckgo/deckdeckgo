export const idlFactory = ({IDL}) => {
  const DeckId = IDL.Text;
  const BucketId = IDL.Principal;
  return IDL.Service({
    del: IDL.Func([DeckId], [IDL.Bool], []),
    deleteDecksAdmin: IDL.Func([IDL.Principal], [], []),
    entries: IDL.Func([], [IDL.Vec(BucketId)], ['query']),
    get: IDL.Func([DeckId], [IDL.Opt(BucketId)], ['query']),
    init: IDL.Func([DeckId], [BucketId], [])
  });
};
export const init = ({IDL}) => {
  return [];
};
