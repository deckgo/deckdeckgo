export const idlFactory = ({IDL}) => {
  const DeckId = IDL.Text;
  const DeckBucketId = IDL.Principal;
  return IDL.Service({
    del: IDL.Func([DeckId], [IDL.Bool], []),
    deleteDecksAdmin: IDL.Func([IDL.Principal], [], []),
    entries: IDL.Func([], [IDL.Vec(DeckBucketId)], ['query']),
    get: IDL.Func([DeckId], [DeckBucketId], ['query']),
    init: IDL.Func([DeckId], [DeckBucketId], [])
  });
};
export const init = ({IDL}) => {
  return [];
};
