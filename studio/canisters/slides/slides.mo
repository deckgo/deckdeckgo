import Iter "mo:base/Iter";
import Principal "mo:base/Principal";

import Error "mo:base/Error";

import Types "../common/types";
import SlidesTypes "./slides.types";
import SlidesStore "./slides.store";

actor Slide {
    type SlideId = Types.SlideId;
    type SlideData = SlidesTypes.SlideData;
    type Slide = SlidesTypes.Slide;
    type UserSlide = SlidesTypes.UserSlide;

    var store: SlidesStore.Store = SlidesStore.Store();

    // Preserve the application state on upgrades
    private stable var slides : [(SlideId, UserSlide)] = [];

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

    public shared({ caller }) func del(slideId : SlideId) : async (Bool) {
        let admin : Principal = Principal.fromActor(Slide);
        
        let exists: Bool = await delAdmin(admin, caller, slideId);

        return exists;
    };

    public func delAdmin(caller: Principal, user: Principal, slideId : SlideId) : async (Bool) {
        let admin : Principal = Principal.fromActor(Slide);

        if (caller != admin) {
            throw Error.reject("This function requires administrative privileges.");
        };

        let exists: Bool = await store.deleteSlide(user, slideId);

        return exists;
    };

    system func preupgrade() {
        slides := Iter.toArray(store.preupgrade().entries());
    };

    system func postupgrade() {
        store.postupgrade(slides);
        slides := [];
    };
}
