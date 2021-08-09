import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";

import Error "mo:base/Error";

import Types "../common/types";
import SlidesTypes "./slides.types";

import Utils "../common/utils";

module {
    type SlideId = Types.SlideId;
    type Slide = SlidesTypes.Slide;
    type OwnerSlide = SlidesTypes.OwnerSlide;
    type ProtectedSlide = SlidesTypes.ProtectedSlide;

    public class Store() {
        private var slides: HashMap.HashMap<SlideId, OwnerSlide> = HashMap.HashMap<SlideId, OwnerSlide>(10, Text.equal, Text.hash);

        public func setSlide(caller: Principal, slide: Slide): ?Text {
            let ({error}): ProtectedSlide = getSlide(caller, slide.slideId);

            switch (error) {
                case (?error) {
                    return ?error;
                };
                case null {
                    slides.put(slide.slideId, {
                        owner = caller;
                        slide = slide;
                    });
                };
            };

            return null;
        };

        public func getSlide(caller: Principal, slideId: SlideId): ProtectedSlide {
            let ownerSlide: ?OwnerSlide = slides.get(slideId);

            switch ownerSlide {
                case (?ownerSlide) {
                    if (Utils.isPrincipalEqual(caller, ownerSlide.owner)) {
                        return {
                            slide = ?ownerSlide;
                            error = null;
                        };
                    };
                };
                case null {
                    return {
                        slide = null;
                        error = null;
                    };
                };
            };

            return {
                slide = null;
                error = ?"User does not have the permission for the slide.";
            };
        };

        public func deleteSlide(user: Principal, slideId : SlideId) : ProtectedSlide {
            let ownerSlide: ProtectedSlide = getSlide(user, slideId);

            let exists: Bool = Option.isSome(ownerSlide.slide);
            if (exists) {
                let removedSlide: ?OwnerSlide = slides.remove(slideId);
            };

            return ownerSlide;
        };

        public func preupgrade(): HashMap.HashMap<SlideId, OwnerSlide> {
            return slides;
        };

        public func postupgrade(entries: [(SlideId, OwnerSlide)]) {
            slides := HashMap.fromIter<SlideId, OwnerSlide>(entries.vals(), 10, Text.equal, Text.hash);
        };
    }

}
