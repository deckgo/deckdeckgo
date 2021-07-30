import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";

import Error "mo:base/Error";

import Types "../common/types";
import SlidesTypes "./slides.types";

module {
    type SlideId = Types.SlideId;
    type SlideData = SlidesTypes.SlideData;
    type Slide = SlidesTypes.Slide;
    type UserSlide = SlidesTypes.UserSlide;

    public class Store() {
        private var slides: HashMap.HashMap<SlideId, UserSlide> = HashMap.HashMap<SlideId, UserSlide>(10, Text.equal, Text.hash);

        public func getSlides(): HashMap.HashMap<SlideId, UserSlide> {
            return slides;
        };

        public func setSlide(user: Principal, slide: Slide): async() {
            let newUserSlide: UserSlide = await initSlide(user, slide);

            slides.put(slide.slideId, newUserSlide);
        };

        private func initSlide(user: Principal, slide: Slide): async (UserSlide) {
            let userSlide: ?UserSlide = await getSlide(user, slide.slideId);

            // If userSlide is null, then it is a new slide
            // If userSlide is not null and there was no error, then it is user slide

            return {
                owner = user;
                slide = slide;
            }
        };

        public func getSlide(user: Principal, slideId: SlideId): async ?UserSlide {
            let userSlide: ?UserSlide = slides.get(slideId);

            switch userSlide {
                case (?userSlide) {
                    await check_permission(user, userSlide);
                };
                case null {
                    return null;
                }
            };

            return userSlide;
        };    

        public func deleteSlide(user: Principal, slideId : SlideId) : async Bool {
            let userSlide: ?UserSlide = await getSlide(user, slideId);

            let exists: Bool = Option.isSome(userSlide);
            if (exists) {
                let removedSlide: ?UserSlide = slides.remove(slideId);
            };
            
            return exists;
        };    

        private func check_permission(user: Principal, userSlide: UserSlide) : async () {
            if (user != userSlide.owner) {
                throw Error.reject("User does not have the permission for the slide.");
            };
        };

        public func postupgrade(entries: [(SlideId, UserSlide)]) {
            slides := HashMap.fromIter<SlideId, UserSlide>(entries.vals(), 10, Text.equal, Text.hash);
        };
    }

}