import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Time "mo:base/Time";

import Types "../common/types";
import UserTypes "../users/users.types";

module {

    type DeckId = Types.DeckId;
    type SlideId = Types.SlideId;
    type Attribute = Types.Attribute;

    type UserSocial = UserTypes.UserSocial;

    public type DeckMetaAuthor = {
        name: Text;
        photo_url: ?Text;
        social: ?UserSocial;
    };

    public type DeckMeta = {
        title: Text;

        description: ?Text;
        tags: ?[Text];

        pathname: ?Text;

        author: ?DeckMetaAuthor;

        published: ?Bool;
        published_at: ?Time.Time;

        feed: ?Bool;

        updated_at: Time.Time;
    };

    public type DeckGitHubRepo = {
        id: Text;
        url: Text;
        name: Text;
        nameWithOwner: Text;
    };

    public type DeckGitHub = {
        repo: ?DeckGitHubRepo;
        publish: Bool;
    };

    public type DeckData = {
        name: Text;

        attributes: ?[Attribute];

        background: ?Text;
        header: ?Text;
        footer: ?Text;

        slides: ?[SlideId];

        meta: ?DeckMeta;

        github: ?DeckGitHub;

        created_at: ?Time.Time;
        updated_at: ?Time.Time;
    };

    public type Deck = {
        deckId: DeckId;
        data: DeckData;
    };

}
