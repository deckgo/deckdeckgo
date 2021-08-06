import Text "mo:base/Text";
import Principal "mo:base/Principal";

module {

    public type DeckId = Text;
    public type SlideId = Text;
    public type UserId = Principal;

    public type Attribute = {
        name: Text;
        value: Text;
    };

}
