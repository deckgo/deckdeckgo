import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Time "mo:base/Time";

import Types "../common/types";

module {

    type SlideId = Types.SlideId;

    public type SlideAttribute = {
        name: Text;
        value: Text;
    };

    public type SlideData = {
        content: ?Text;

        template: Text;
        scope: ?Text;

        attributes: ?[SlideAttribute];

        created_at: ?Time.Time;
        updated_at: ?Time.Time;
    };

    public type Slide = {
        id: SlideId;
        data: SlideData;
    };

    public type UserSlide = {
        owner: Principal;
        slide: Slide;
    };

}
