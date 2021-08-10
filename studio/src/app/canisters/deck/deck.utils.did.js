export const idlFactory = ({IDL}) => {
  const UserId = IDL.Principal;
  const UserId__1 = IDL.Principal;
  const SlideId__2 = IDL.Text;
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
  const SlideId__1 = IDL.Text;
  const Attribute__1 = IDL.Record({value: IDL.Text, name: IDL.Text});
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
    slides: IDL.Opt(IDL.Vec(SlideId__1)),
    attributes: IDL.Opt(IDL.Vec(Attribute__1)),
    github: IDL.Opt(DeckGitHub),
    footer: IDL.Opt(IDL.Text),
    header: IDL.Opt(IDL.Text)
  });
  const DeckId = IDL.Text;
  const Deck = IDL.Record({data: DeckData, deckId: DeckId});
  const Attribute = IDL.Record({value: IDL.Text, name: IDL.Text});
  const SlideData = IDL.Record({
    updated_at: IDL.Opt(Time),
    content: IDL.Opt(IDL.Text),
    created_at: IDL.Opt(Time),
    scope: IDL.Opt(IDL.Text),
    attributes: IDL.Opt(IDL.Vec(Attribute)),
    template: IDL.Text
  });
  const SlideId = IDL.Text;
  const Slide = IDL.Record({data: SlideData, slideId: SlideId});
  const DeckBucket = IDL.Service({
    del: IDL.Func([], [IDL.Bool], []),
    delAdmin: IDL.Func([UserId__1], [IDL.Bool], []),
    delSlide: IDL.Func([SlideId__2], [IDL.Bool], []),
    get: IDL.Func([], [Deck], ['query']),
    getSlide: IDL.Func([SlideId__2], [Slide], ['query']),
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
