export const idlFactory = ({IDL}) => {
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
  const DeckMetaAuthor = IDL.Record({
    photo_url: IDL.Opt(IDL.Text),
    social: IDL.Opt(UserSocial),
    name: IDL.Text
  });
  const DeckMeta = IDL.Record({
    title: IDL.Text,
    updated_at: Time,
    feed: IDL.Opt(IDL.Bool),
    published: IDL.Opt(IDL.Bool),
    tags: IDL.Opt(IDL.Vec(IDL.Text)),
    pathname: IDL.Opt(IDL.Text),
    description: IDL.Opt(IDL.Text),
    published_at: IDL.Opt(Time),
    author: IDL.Opt(DeckMetaAuthor)
  });
  const SlideId = IDL.Text;
  const Attribute = IDL.Record({value: IDL.Text, name: IDL.Text});
  const DeckGitHubRepo = IDL.Record({
    id: IDL.Text,
    url: IDL.Text,
    name: IDL.Text,
    nameWithOwner: IDL.Text
  });
  const DeckGitHub = IDL.Record({
    repo: IDL.Opt(DeckGitHubRepo),
    publish: IDL.Bool
  });
  const DeckData = IDL.Record({
    updated_at: IDL.Opt(Time),
    background: IDL.Opt(IDL.Text),
    meta: IDL.Opt(DeckMeta),
    name: IDL.Text,
    created_at: IDL.Opt(Time),
    slides: IDL.Opt(IDL.Vec(SlideId)),
    attributes: IDL.Opt(IDL.Vec(Attribute)),
    github: IDL.Opt(DeckGitHub),
    footer: IDL.Opt(IDL.Text),
    header: IDL.Opt(IDL.Text)
  });
  const DeckId__1 = IDL.Text;
  const Deck = IDL.Record({data: DeckData, deckId: DeckId__1});
  const DeckBucket = IDL.Service({
    del: IDL.Func([IDL.Bool], [IDL.Bool], []),
    get: IDL.Func([], [Deck], ['query']),
    set: IDL.Func([Deck], [], [])
  });
  const DeckId = IDL.Text;
  return IDL.Service({
    entries: IDL.Func([], [IDL.Vec(DeckBucket)], ['query']),
    entriesAdmin: IDL.Func([IDL.Principal], [IDL.Vec(DeckBucket)], []),
    get: IDL.Func([DeckId], [DeckBucket], ['query']),
    init: IDL.Func([DeckId], [DeckBucket], [])
  });
};
export const init = ({IDL}) => {
  return [];
};
