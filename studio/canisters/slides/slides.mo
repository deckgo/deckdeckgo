import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Text "mo:base/Text";

actor Slide {

    private type SlideId = Text;

    private type SlideData = {
        content: ?Text;
    };

    private type Slide = {
        id: SlideId;
        data: SlideData;
    };

    private type UserSlide = {
        owner: Principal;
        slide: Slide;
    };

    // Preserve the application state on upgrades
    private stable var entries : [(SlideId, UserSlide)] = [];

    private let slides = HashMap.fromIter<SlideId, UserSlide>(entries.vals(), 10, Text.equal, Text.hash);

    public shared({ caller }) func set(slide: Slide) {
      slides.put(slide.id, {
          owner = caller;
          slide;
      });
    };

    public shared({ caller }) func get(slideId: SlideId): async(?UserSlide) {
      return slides.get(slideId)
    };

    system func preupgrade() {
        entries := Iter.toArray(slides.entries());
    };

    system func postupgrade() {
        entries := [];
    };
}
