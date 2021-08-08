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
    type Deck = DecksTypes.Deck;
    type OwnerDeck = DecksTypes.OwnerDeck;

    public class Store() {
        private var decks: HashMap.HashMap<Principal, HashMap.HashMap<DeckId, OwnerDeck>> = HashMap.HashMap<Principal, HashMap.HashMap<DeckId, OwnerDeck>>(10, Utils.isPrincipalEqual, Principal.hash);

        public func setDeck(user: Principal, deck: Deck): async() {
            let newOwnerDeck: OwnerDeck = await initDeck(user, deck);

            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeck> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    setOwnerDeck(user, newOwnerDeck, ownerDecks);
                };
                case null {
                    let ownerDecks: HashMap.HashMap<DeckId, OwnerDeck> = HashMap.HashMap<DeckId, OwnerDeck>(10, Text.equal, Text.hash);

                    setOwnerDeck(user, newOwnerDeck, ownerDecks);
                }
            };
        };

        private func setOwnerDeck(user: Principal, newOwnerDeck: OwnerDeck, ownerDecks: HashMap.HashMap<DeckId, OwnerDeck>) {
            ownerDecks.put(newOwnerDeck.deck.deckId, newOwnerDeck);
            decks.put(user, ownerDecks);
        };

        private func initDeck(user: Principal, deck: Deck): async (OwnerDeck) {
            let ownerDeck: ?OwnerDeck = await getDeck(user, deck.deckId);

            // If ownerDeck is null, then it is a new deck
            // If ownerDeck is not null and there was no error, then it is user deck

            return {
                owner = user;
                deck = deck;
            }
        };

        public func getDeck(user: Principal, deckId: DeckId): async ?OwnerDeck {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeck> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    let ownerDeck: ?OwnerDeck = await getOwnerDeck(user, deckId, ownerDecks);
                    return ownerDeck;
                };
                case null {
                    return null;
                }
            };
        };

        public func getDecks(user: Principal): async ([Deck]) {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeck> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    var results: ([Deck]) = [];

                    for ((deckId: DeckId, value: OwnerDeck) in ownerDecks.entries()) {
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

        private func getOwnerDeck(user: Principal, deckId: DeckId, ownerDecks: HashMap.HashMap<DeckId, OwnerDeck>): async ?OwnerDeck {
            let ownerDeck: ?OwnerDeck = ownerDecks.get(deckId);

            switch ownerDeck {
                case (?ownerDeck) {
                    await check_permission(user, ownerDeck);
                };
                case null {
                    return null;
                }
            };

            return ownerDeck;
        };

        public func deleteDeck(user: Principal, deckId : DeckId, slides: Bool) : async Bool {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeck> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    let ownerDeck: ?OwnerDeck = await getOwnerDeck(user, deckId, ownerDecks);

                    switch ownerDeck {
                        case (?ownerDeck) {
                            await deleteSlides(user, ownerDeck.deck);

                            let removedDeck: ?OwnerDeck = ownerDecks.remove(deckId);
                            decks.put(user, ownerDecks);

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
                        let slideExists: Bool = await Slides.delAdmin(user, slideId);
                    };
                };
                case null {}
            };
        };

        private func check_permission(user: Principal, ownerDeck: OwnerDeck) : async () {
            if (user != ownerDeck.owner) {
                throw Error.reject("User does not have the permission for the deck.");
            };
        };


        public func preupgrade(): HashMap.HashMap<Principal, [(DeckId, OwnerDeck)]> {
            let entries : HashMap.HashMap<Principal, [(DeckId, OwnerDeck)]> = HashMap.HashMap<Principal, [(DeckId, OwnerDeck)]>(10, Utils.isPrincipalEqual, Principal.hash);

            for ((key: Principal, value: HashMap.HashMap<DeckId, OwnerDeck>) in decks.entries()) {
                let ownerDecks : [(DeckId, OwnerDeck)] = Iter.toArray<(DeckId, OwnerDeck)>(value.entries());
                entries.put(key, ownerDecks);
            };

            return entries;
        };

        public func postupgrade(entries: [(Principal, [(DeckId, OwnerDeck)])]) {
            for ((key: Principal, value: [(DeckId, OwnerDeck)]) in entries.vals()) {
                let ownerDecks: HashMap.HashMap<DeckId, OwnerDeck> = HashMap.fromIter<DeckId, OwnerDeck>(Iter.fromArray<(DeckId, OwnerDeck)>(value), 10, Text.equal, Text.hash);

                decks.put(key, ownerDecks);
            };
        };
    }

}
