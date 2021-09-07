import Text "mo:base/Text";
import Time "mo:base/Time";
import Blob "mo:base/Blob";

import Types "../common/types";

module {

    type SlideId = Types.SlideId;

    public type Slide = {
        slideId: SlideId;

        data: Blob;

        created_at: Time.Time;
        updated_at: Time.Time;
    };

}
