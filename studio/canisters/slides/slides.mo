import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Option "mo:base/Option";

import Error "mo:base/Error";

import Types "../common/types";
import SlidesTypes "./slides.types";
import SlidesStore "./slides.store";

actor Slide {
    type SlideId = Types.SlideId;
    type SlideData = SlidesTypes.SlideData;
    type Slide = SlidesTypes.Slide;
    type OwnerSlide = SlidesTypes.OwnerSlide;
    type ProtectedSlide = SlidesTypes.ProtectedSlide;

    var store: SlidesStore.Store = SlidesStore.Store();

    // Preserve the application state on upgrades
    private stable var slides : [(SlideId, OwnerSlide)] = [];

    public shared({ caller }) func set(slide: Slide): async() {
        let error: ?Text = store.setSlide(caller, slide);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {};
        };
    };

    public shared query({ caller }) func get(slideId : SlideId) : async Slide {
        let ({error; slide;}): ProtectedSlide = store.getSlide(caller, slideId);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                switch (slide) {
                    case (?slide) {
                        return slide.slide;
                    };
                    case null {
                        throw Error.reject("Slide not found.");
                    };
                };
            };
        };
    };

    public shared({ caller }) func del(slideId : SlideId) : async (Bool) {
        let slide: ProtectedSlide = store.deleteSlide(caller, slideId);

        switch (slide.error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                let exists: Bool = Option.isSome(slide.slide);
                return exists;
            };
        };
    };

    public func delAdmin(user: Principal, slideId: SlideId) : async Bool {
        let slide: ProtectedSlide = store.deleteSlide(user, slideId);

        switch (slide.error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                let exists: Bool = Option.isSome(slide.slide);
                return exists;
            };
        };
    };

    system func preupgrade() {
        slides := Iter.toArray(store.preupgrade().entries());
    };

    system func postupgrade() {
        store.postupgrade(slides);
        slides := [];
    };
}
