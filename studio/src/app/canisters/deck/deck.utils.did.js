export const idlFactory = ({IDL}) => {
  const UserId = IDL.Principal;
  const SlideId__1 = IDL.Text;
  const Time = IDL.Int;
  const DeckId = IDL.Text;
  const Deck = IDL.Record({
    updated_at: Time,
    data: IDL.Vec(IDL.Nat8),
    deckId: DeckId,
    created_at: Time
  });
  const SlideId = IDL.Text;
  const Slide = IDL.Record({
    updated_at: Time,
    data: IDL.Vec(IDL.Nat8),
    created_at: Time,
    slideId: SlideId
  });
  const DeckBucket = IDL.Service({
    delSlide: IDL.Func([SlideId__1], [IDL.Bool], []),
    get: IDL.Func([], [Deck], ['query']),
    getSlide: IDL.Func([SlideId__1], [Slide], ['query']),
    id: IDL.Func([], [IDL.Principal], ['query']),
    set: IDL.Func([Deck], [], []),
    setSlide: IDL.Func([Slide], [], [])
  });
  return DeckBucket;
};
export const init = ({IDL}) => {
  const UserId = IDL.Principal;
  return [UserId];
};
