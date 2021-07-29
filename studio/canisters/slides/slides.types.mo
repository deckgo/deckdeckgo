import Text "mo:base/Text";
import Principal "mo:base/Principal";

module {

    public type SlideId = Text;

    public type SlideData = {
        content: ?Text;
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