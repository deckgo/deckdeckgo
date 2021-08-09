import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Time "mo:base/Time";

import Types "../common/types";

module {

    type SlideId = Types.SlideId;
    type DeckId = Types.DeckId;
    type Attribute = Types.Attribute;

    public type SlideData = {
        content: ?Text;

        template: Text;
        scope: ?Text;

        attributes: ?[Attribute];

        created_at: ?Time.Time;
        updated_at: ?Time.Time;
    };

    public type Slide = {
        slideId: SlideId;
        deckId: DeckId;
        data: SlideData;
    };

    public type OwnerSlide = {
        owner: Principal;
        slide: Slide;
    };

    public type ProtectedSlide = {
        slide: ?OwnerSlide;
        error: ?Text;
    };

}
