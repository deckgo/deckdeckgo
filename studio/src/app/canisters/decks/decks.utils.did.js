export const idlFactory = ({IDL}) => {
  const DeckBucketId = IDL.Principal;
  const DeckId = IDL.Text;
  return IDL.Service({
    entries: IDL.Func([], [IDL.Vec(DeckBucketId)], ['query']),
    entriesAdmin: IDL.Func([IDL.Principal], [IDL.Vec(DeckBucketId)], []),
    get: IDL.Func([DeckId], [DeckBucketId], ['query']),
    init: IDL.Func([DeckId], [DeckBucketId], [])
  });
};
export const init = ({IDL}) => {
  return [];
};
