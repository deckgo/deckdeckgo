export const idlFactory = ({IDL}) => {
  const SlideId__1 = IDL.Text;
  const SlideId = IDL.Text;
  const SlideData = IDL.Record({content: IDL.Opt(IDL.Text)});
  const Slide = IDL.Record({id: SlideId, data: SlideData});
  return IDL.Service({
    del: IDL.Func([SlideId__1], [], []),
    get: IDL.Func([SlideId__1], [Slide], []),
    set: IDL.Func([Slide], [], ['oneway'])
  });
};
export const init = ({IDL}) => {
  return [];
};
