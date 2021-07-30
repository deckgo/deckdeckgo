import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Array "mo:base/Array";

import Error "mo:base/Error";

import Types "../common/types";
import DecksTypes "./decks.types";

module {
    type DeckId = Types.DeckId;
    type DeckData = DecksTypes.DeckData;
    type Deck = DecksTypes.Deck;
    type UserDeck = DecksTypes.UserDeck;

    type InitUserDeck = {
        new: Bool;
        userDeck: UserDeck;
    };

    public class Store() {
        private var entries: HashMap.HashMap<DeckId, UserDeck> = HashMap.HashMap<DeckId, UserDeck>(10, Text.equal, Text.hash);

        private func isEq(x: Principal, y: Principal): Bool { x == y };

        private var index: HashMap.HashMap<Principal, [DeckId]> = HashMap.HashMap<Principal, [DeckId]>(10, isEq, Principal.hash);

        public func getDecks(): HashMap.HashMap<DeckId, UserDeck> {
            return entries;
        };

        public func getIndex(): HashMap.HashMap<Principal, [DeckId]> {
            return index;
        };

        public func setDeck(user: Principal, deck: Deck): async() {
            let newUserDeck: InitUserDeck = await initDeck(user, deck);

            entries.put(deck.deckId, newUserDeck.userDeck);

            if (newUserDeck.new) {
                indexDeck(user, deck.deckId);
            }
        };

        private func initDeck(user: Principal, deck: Deck): async (InitUserDeck) {
            let userDeck: ?UserDeck = await getDeck(user, deck.deckId);

            // If userDeck is null, then it is a new deck
            // If userDeck is not null and there was no error, then it is user deck

            return {
                new = Option.isSome(userDeck);
                userDeck = {
                    owner = user;
                    deck = deck;
                };
            }
        };

        private func indexDeck(user: Principal, deckId: DeckId) {
            let decksIndex: ?[DeckId] = index.get(user);

            switch decksIndex {
                case (?decksIndex) {
                    let newIndex: [DeckId] = Array.append(decksIndex, [deckId]);
                    index.put(user, newIndex);
                };
                case null {
                    index.put(user, [deckId]);
                }
            };
        };

        public func getDeck(user: Principal, deckId: DeckId): async ?UserDeck {
            let userDeck: ?UserDeck = entries.get(deckId);

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

        public func deleteDeck(user: Principal, deckId : DeckId) : async Bool {
            let userDeck: ?UserDeck = await getDeck(user, deckId);

            let exists: Bool = Option.isSome(userDeck);
            if (exists) {
                let removedDeck: ?UserDeck = entries.remove(deckId);
            };

            return exists;
        };

        private func check_permission(user: Principal, userDeck: UserDeck) : async () {
            if (user != userDeck.owner) {
                throw Error.reject("User does not have the permission for the deck.");
            };
        };

        public func postupgrade(decks: [(DeckId, UserDeck)], decksIndex: [(Principal, [DeckId])]) {
            entries := HashMap.fromIter<DeckId, UserDeck>(decks.vals(), 10, Text.equal, Text.hash);
            index := HashMap.fromIter<Principal, [DeckId]>(decksIndex.vals(), 10, isEq, Principal.hash);
        };

    }

}
