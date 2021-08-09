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
    type ProtectedDeck = DecksTypes.ProtectedDeck;
    type ProtectedDecks = DecksTypes.ProtectedDecks;

    public class Store() {
        private var decks: HashMap.HashMap<Principal, HashMap.HashMap<DeckId, OwnerDeck>> = HashMap.HashMap<Principal, HashMap.HashMap<DeckId, OwnerDeck>>(10, Utils.isPrincipalEqual, Principal.hash);

        public func setDeck(user: Principal, deck: Deck): ?Text {
            let ({error}): ProtectedDeck = getDeck(user, deck.deckId);

            switch (error) {
                case (?error) {
                    return ?error;
                };
                case null {
                    let newOwnerDeck = {
                        owner = user;
                        deck = deck;
                    };

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
            };

            return null;
        };

        private func setOwnerDeck(user: Principal, newOwnerDeck: OwnerDeck, ownerDecks: HashMap.HashMap<DeckId, OwnerDeck>) {
            ownerDecks.put(newOwnerDeck.deck.deckId, newOwnerDeck);
            decks.put(user, ownerDecks);
        };

        public func getDeck(user: Principal, deckId: DeckId): ProtectedDeck {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeck> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    let ownerDeck: ProtectedDeck = getOwnerDeck(user, deckId, ownerDecks);
                    return ownerDeck;
                };
                case null {
                    return {
                        deck = null;
                        error = null;
                    };
                };
            };
        };

        public func getDecks(user: Principal): ProtectedDecks {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeck> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    var results: ([Deck]) = [];

                    for ((deckId: DeckId, value: OwnerDeck) in ownerDecks.entries()) {
                        if (Utils.isPrincipalNotEqual(user, value.owner)) {
                            return {
                                decks = [];
                                error = ?"User does not have the permission for one of the deck.";
                            };
                        };

                        results := Array.append(results, [value.deck]);
                    };

                    return {
                        decks = results;
                        error = null;
                    };
                };
                case null {
                    return {
                        decks = [];
                        error = null;
                    }
                }
            };
        };

        private func getOwnerDeck(user: Principal, deckId: DeckId, ownerDecks: HashMap.HashMap<DeckId, OwnerDeck>): ProtectedDeck {
            let ownerDeck: ?OwnerDeck = ownerDecks.get(deckId);

            switch ownerDeck {
                case (?ownerDeck) {
                    if (Utils.isPrincipalEqual(user, ownerDeck.owner)) {
                        return {
                            deck = ?ownerDeck;
                            error = null;
                        };
                    };
                };
                case null {
                    return {
                        deck = null;
                        error = null;
                    };
                };
            };

            return {
                deck = null;
                error = ?"User does not have the permission for the deck.";
            };
        };

        public func deleteDeck(user: Principal, deckId : DeckId, slides: Bool) : async (ProtectedDeck) {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeck> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    let protectedDeck: ProtectedDeck = getOwnerDeck(user, deckId, ownerDecks);

                    switch (protectedDeck.deck) {
                        case (?deck) {
                            await deleteSlides(user, deck.deck);

                            let removedDeck: ?OwnerDeck = ownerDecks.remove(deckId);
                            decks.put(user, ownerDecks);
                        };
                        case null {};
                    };

                    return protectedDeck;
                };
                case null {
                    return {
                        deck = null;
                        error = null;
                    };
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
