export const idlFactory = ({IDL}) => {
  const UserId__1 = IDL.Principal;
  const UserId = IDL.Principal;
  const Time = IDL.Int;
  const UserSocial = IDL.Record({
    dev: IDL.Opt(IDL.Text),
    linkedin: IDL.Opt(IDL.Text),
    twitter: IDL.Opt(IDL.Text),
    custom_logo_url: IDL.Opt(IDL.Text),
    custom: IDL.Opt(IDL.Text),
    github: IDL.Opt(IDL.Text),
    medium: IDL.Opt(IDL.Text)
  });
  const UserData = IDL.Record({
    bio: IDL.Opt(IDL.Text),
    updated_at: IDL.Opt(Time),
    photo_url: IDL.Opt(IDL.Text),
    social: IDL.Opt(UserSocial),
    name: IDL.Opt(IDL.Text),
    created_at: IDL.Opt(Time),
    email: IDL.Opt(IDL.Text),
    newsletter: IDL.Opt(IDL.Bool)
  });
  const User = IDL.Record({userId: UserId, data: UserData});
  return IDL.Service({
    del: IDL.Func([UserId__1], [IDL.Bool], []),
    get: IDL.Func([UserId__1], [User], ['query']),
    getUserId: IDL.Func([], [UserId__1], ['query']),
    set: IDL.Func([User], [], ['oneway'])
  });
};
export const init = ({IDL}) => {
  return [];
};
