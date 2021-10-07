export const idlFactory = ({IDL}) => {
  const BucketId = IDL.Principal;
  const DeckId = IDL.Text;
  return IDL.Service({
    deckEntries: IDL.Func([], [IDL.Vec(BucketId)], ['query']),
    delDeck: IDL.Func([DeckId], [IDL.Bool], []),
    deleteDecksAdmin: IDL.Func([IDL.Principal], [], []),
    getDeck: IDL.Func([DeckId], [IDL.Opt(BucketId)], ['query']),
    initDeck: IDL.Func([DeckId], [BucketId], [])
  });
};
export const init = ({IDL}) => {
  return [];
};
