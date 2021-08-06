import Text "mo:base/Text";
import Time "mo:base/Time";

import Types "../common/types";

module {
    type UserId = Types.UserId;

    public type UserSocial = {
        twitter: ?Text;
        linkedin: ?Text;
        dev: ?Text;
        medium: ?Text;
        github: ?Text;

        custom: ?Text;
        custom_logo_url: ?Text;
    };

    public type UserData = {
        name: ?Text;
        email: ?Text;
        newsletter: ?Bool;
        photo_url: ?Text;

        social: ?UserSocial;

        bio: ?Text;

        created_at: ?Time.Time;
        updated_at: ?Time.Time;
    };

    public type User = {
        userId: UserId;
        data: UserData;
    };

    public type OwnerUser = {
        owner: UserId;
        user: User;
    };

    public type ProtectedUser = {
        user: ?OwnerUser;
        error: ?Text;
    };
}
