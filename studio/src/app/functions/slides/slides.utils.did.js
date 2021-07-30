export const idlFactory = ({IDL}) => {
  const SlideId__1 = IDL.Text;
  const SlideId = IDL.Text;
  const Time = IDL.Int;
  const SlideAttribute = IDL.Record({value: IDL.Text, name: IDL.Text});
  const SlideData = IDL.Record({
    updated_at: IDL.Opt(Time),
    content: IDL.Opt(IDL.Text),
    created_at: IDL.Opt(Time),
    scope: IDL.Opt(IDL.Text),
    attributes: IDL.Opt(IDL.Vec(SlideAttribute)),
    template: IDL.Text
  });
  const Slide = IDL.Record({id: SlideId, data: SlideData});
  return IDL.Service({
    del: IDL.Func([SlideId__1], [IDL.Bool], []),
    get: IDL.Func([SlideId__1], [Slide], []),
    set: IDL.Func([Slide], [], ['oneway'])
  });
};
export const init = ({IDL}) => {
  return [];
};
