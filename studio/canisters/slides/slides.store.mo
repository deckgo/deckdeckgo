import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";

import Error "mo:base/Error";

import Types "../common/types";
import SlidesTypes "./slides.types";

module {
    type SlideId = Types.SlideId;
    type Slide = SlidesTypes.Slide;
    type OwnerSlide = SlidesTypes.OwnerSlide;

    public class Store() {
        private var slides: HashMap.HashMap<SlideId, OwnerSlide> = HashMap.HashMap<SlideId, OwnerSlide>(10, Text.equal, Text.hash);

        public func setSlide(user: Principal, slide: Slide): async() {
            let newOwnerSlide: OwnerSlide = await initSlide(user, slide);

            slides.put(slide.slideId, newOwnerSlide);
        };

        private func initSlide(user: Principal, slide: Slide): async (OwnerSlide) {
            let ownerSlide: ?OwnerSlide = await getSlide(user, slide.slideId);

            // If ownerSlide is null, then it is a new slide
            // If ownerSlide is not null and there was no error, then it is user slide

            return {
                owner = user;
                slide = slide;
            }
        };

        public func getSlide(user: Principal, slideId: SlideId): async ?OwnerSlide {
            let ownerSlide: ?OwnerSlide = slides.get(slideId);

            switch ownerSlide {
                case (?ownerSlide) {
                    await check_permission(user, ownerSlide);
                };
                case null {
                    return null;
                }
            };

            return ownerSlide;
        };

        public func deleteSlide(user: Principal, slideId : SlideId) : async Bool {
            let ownerSlide: ?OwnerSlide = await getSlide(user, slideId);

            let exists: Bool = Option.isSome(ownerSlide);
            if (exists) {
                let removedSlide: ?OwnerSlide = slides.remove(slideId);
            };

            return exists;
        };

        private func check_permission(user: Principal, ownerSlide: OwnerSlide) : async () {
            if (user != ownerSlide.owner) {
                throw Error.reject("User does not have the permission for the slide.");
            };
        };

        public func preupgrade(): HashMap.HashMap<SlideId, OwnerSlide> {
            return slides;
        };

        public func postupgrade(entries: [(SlideId, OwnerSlide)]) {
            slides := HashMap.fromIter<SlideId, OwnerSlide>(entries.vals(), 10, Text.equal, Text.hash);
        };
    }

}
