export const idlFactory = ({IDL}) => {
  const UserId = IDL.Principal;
  const Time = IDL.Int;
  const Data = IDL.Record({
    id: IDL.Text,
    updated_at: Time,
    data: IDL.Vec(IDL.Nat8),
    created_at: Time
  });
  const DataBucket = IDL.Service({
    del: IDL.Func([IDL.Text], [], []),
    get: IDL.Func([IDL.Text], [Data], ['query']),
    list: IDL.Func([IDL.Opt(IDL.Text)], [IDL.Vec(Data)], ['query']),
    set: IDL.Func([IDL.Text, Data], [], []),
    transferCycles: IDL.Func([], [], [])
  });
  return DataBucket;
};
export const init = ({IDL}) => {
  const UserId = IDL.Principal;
  return [UserId];
};
