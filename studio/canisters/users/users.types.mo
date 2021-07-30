import Text "mo:base/Text";

module {
    public type UserSocial = {
        twitter: ?Text;
        linkedin: ?Text;
        dev: ?Text;
        medium: ?Text;
        github: ?Text;

        custom: ?Text;
        custom_logo_url: ?Text;
    };
}
