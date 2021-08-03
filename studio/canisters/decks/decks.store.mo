import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Iter "mo:base/Iter";
import Array "mo:base/Array";

import Error "mo:base/Error";

import Types "../common/types";
import DecksTypes "./decks.types";

import Utils "../common/utils";

import Slides "canister:slides";

module {
    type DeckId = Types.DeckId;
    type SlideId = Types.SlideId;
    type DeckData = DecksTypes.DeckData;
    type Deck = DecksTypes.Deck;
    type UserDeck = DecksTypes.UserDeck;

    public class Store() {
        let utils: Utils.Utils = Utils.Utils();

        private var decks: HashMap.HashMap<Principal, HashMap.HashMap<DeckId, UserDeck>> = HashMap.HashMap<Principal, HashMap.HashMap<DeckId, UserDeck>>(10, utils.isPrincipalEqual, Principal.hash);

        public func setDeck(user: Principal, deck: Deck): async() {
            let newUserDeck: UserDeck = await initDeck(user, deck);

            let userDecks: ?HashMap.HashMap<DeckId, UserDeck> = decks.get(user);

            switch userDecks {
                case (?userDecks) {
                    setUserDeck(user, newUserDeck, userDecks);
                };
                case null {
                    let userDecks: HashMap.HashMap<DeckId, UserDeck> = HashMap.HashMap<DeckId, UserDeck>(10, Text.equal, Text.hash);

                    setUserDeck(user, newUserDeck, userDecks);
                }
            };
        };

        private func setUserDeck(user: Principal, newUserDeck: UserDeck, userDecks: HashMap.HashMap<DeckId, UserDeck>) {
            userDecks.put(newUserDeck.deck.deckId, newUserDeck);
            decks.put(user, userDecks);
        };

        private func initDeck(user: Principal, deck: Deck): async (UserDeck) {
            let userDeck: ?UserDeck = await getDeck(user, deck.deckId);

            // If userDeck is null, then it is a new deck
            // If userDeck is not null and there was no error, then it is user deck

            return {
                owner = user;
                deck = deck;
            }
        };

        public func getDeck(user: Principal, deckId: DeckId): async ?UserDeck {
            let userDecks: ?HashMap.HashMap<DeckId, UserDeck> = decks.get(user);

            switch userDecks {
                case (?userDecks) {
                    let userDeck: ?UserDeck = await getUserDeck(user, deckId, userDecks);
                    return userDeck;
                };
                case null {
                    return null;
                }
            };
        };

        public func getDecks(user: Principal): async ([Deck]) {
            let userDecks: ?HashMap.HashMap<DeckId, UserDeck> = decks.get(user);

            switch userDecks {
                case (?userDecks) {
                    var results: ([Deck]) = [];

                    for ((deckId: DeckId, value: UserDeck) in userDecks.entries()) {
                        // Check permissions on each decks
                        await check_permission(user, value);

                        results := Array.append(results, [value.deck]);
                    };

                    return results;
                };
                case null {
                    return [];
                }
            };
        };

        private func getUserDeck(user: Principal, deckId: DeckId, userDecks: HashMap.HashMap<DeckId, UserDeck>): async ?UserDeck {
            let userDeck: ?UserDeck = userDecks.get(deckId);

            switch userDeck {
                case (?userDeck) {
                    await check_permission(user, userDeck);
                };
                case null {
                    return null;
                }
            };

            return userDeck;
        };

        public func deleteDeck(user: Principal, deckId : DeckId, slides: Bool) : async Bool {
            let userDecks: ?HashMap.HashMap<DeckId, UserDeck> = decks.get(user);

            switch userDecks {
                case (?userDecks) {
                    let userDeck: ?UserDeck = await getUserDeck(user, deckId, userDecks);

                    switch userDeck {
                        case (?userDeck) {
                            await deleteSlides(user, userDeck.deck);

                            let removedDeck: ?UserDeck = userDecks.remove(deckId);
                            decks.put(user, userDecks);

                            return true;
                        };
                        case null {
                            return false;
                        }
                    };
                };
                case null {
                    return false;
                }
            };
        };

        private func deleteSlides(user: Principal, deck: Deck): async () {
            let slides: ?[SlideId] = deck.data.slides;

            switch (slides) {
                case (?slides) {
                    for ((slideId: Text) in slides.vals()) {
                        let slideExists: Bool = await Slides.deleteSlide(user, slideId);
                    };
                };
                case null {}
            };
        };

        private func check_permission(user: Principal, userDeck: UserDeck) : async () {
            if (user != userDeck.owner) {
                throw Error.reject("User does not have the permission for the deck.");
            };
        };


        public func preupgrade(): HashMap.HashMap<Principal, [(DeckId, UserDeck)]> {
            let entries : HashMap.HashMap<Principal, [(DeckId, UserDeck)]> = HashMap.HashMap<Principal, [(DeckId, UserDeck)]>(10, utils.isPrincipalEqual, Principal.hash);

            for ((key: Principal, value: HashMap.HashMap<DeckId, UserDeck>) in decks.entries()) {
                let userDecks : [(DeckId, UserDeck)] = Iter.toArray<(DeckId, UserDeck)>(value.entries());
                entries.put(key, userDecks);
            };

            return entries;
        };

        public func postupgrade(entries: [(Principal, [(DeckId, UserDeck)])]) {
            for ((key: Principal, value: [(DeckId, UserDeck)]) in entries.vals()) {
                let userDecks: HashMap.HashMap<DeckId, UserDeck> = HashMap.fromIter<DeckId, UserDeck>(Iter.fromArray<(DeckId, UserDeck)>(value), 10, Text.equal, Text.hash);

                decks.put(key, userDecks);
            };
        };
    }

}
