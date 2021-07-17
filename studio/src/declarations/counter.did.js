export const idlFactory = ({ IDL }) => {
  return IDL.Service({
    'getValue' : IDL.Func([], [IDL.Nat], ['query']),
    'increment' : IDL.Func([], [], []),
  });
};
export const init = ({ IDL }) => { return []; };