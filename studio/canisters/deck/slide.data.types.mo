import Text "mo:base/Text";
import Time "mo:base/Time";

import Types "../common/types";

module {

    type SlideId = Types.SlideId;
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
        data: SlideData;
    };

}
