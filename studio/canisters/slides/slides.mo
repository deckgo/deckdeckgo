import Iter "mo:base/Iter";

import Error "mo:base/Error";

import Types "./slides.types";
import Store "./slides.store";

actor Slide {
    type SlideId = Types.SlideId;
    type SlideData = Types.SlideData;
    type Slide = Types.Slide;
    type UserSlide = Types.UserSlide;

    var store: Store.Store = Store.Store();

    // Preserve the application state on upgrades
    private stable var entries : [(SlideId, UserSlide)] = [];

    public shared({ caller }) func set(slide: Slide) {
        await store.setSlide(caller, slide);
    };

    public shared({ caller }) func get(slideId : SlideId) : async Slide {
        let userSlide: ?UserSlide = await store.getSlide(caller, slideId);

        switch userSlide {
            case (?userSlide) {
                return userSlide.slide;
            };
            case null {
                throw Error.reject("Slide not found.");
            };
        };
    };

    public shared({ caller }) func del(slideId : SlideId) : async () {
        let exists: Bool = await store.deleteSlide(caller, slideId);

        switch exists {
            case true {
                return;
            };
            case false {
                throw Error.reject("Slide not found.");
            };
        };
    };

    system func preupgrade() {
        entries := Iter.toArray(store.getSlides().entries());
    };

    system func postupgrade() {
        store.postupgrade(entries);
        entries := [];
    };
}
