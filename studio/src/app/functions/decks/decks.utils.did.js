export const idlFactory = ({IDL}) => {
  const DeckId = IDL.Text;
  const DeckData = IDL.Record({
    name: IDL.Text,
    header: IDL.Opt(IDL.Text)
  });
  const Deck = IDL.Record({id: DeckId, data: DeckData});
  return IDL.Service({
    del: IDL.Func([DeckId], [IDL.Bool], []),
    get: IDL.Func([DeckId], [Deck], []),
    set: IDL.Func([Deck], [], [])
  });
};
export const init = ({IDL}) => {
  return [];
};
