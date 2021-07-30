import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";

import Error "mo:base/Error";

import Types "../common/types";
import DecksTypes "./decks.types";

module {
    type DeckId = Types.DeckId;
    type DeckData = DecksTypes.DeckData;
    type Deck = DecksTypes.Deck;
    type UserDeck = DecksTypes.UserDeck;

    public class Store() {
        private var decks: HashMap.HashMap<DeckId, UserDeck> = HashMap.HashMap<DeckId, UserDeck>(10, Text.equal, Text.hash);

        public func getDecks(): HashMap.HashMap<DeckId, UserDeck> {
            return decks;
        };

        public func setDeck(user: Principal, deck: Deck): async() {
            let newUserDeck: UserDeck = await initDeck(user, deck);

            decks.put(deck.deckId, newUserDeck);
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
            let userDeck: ?UserDeck = decks.get(deckId);

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
                let removedDeck: ?UserDeck = decks.remove(deckId);
            };

            return exists;
        };

        private func check_permission(user: Principal, userDeck: UserDeck) : async () {
            if (user != userDeck.owner) {
                throw Error.reject("User does not have the permission for the deck.");
            };
        };

        public func postupgrade(entries: [(DeckId, UserDeck)]) {
            decks := HashMap.fromIter<DeckId, UserDeck>(entries.vals(), 10, Text.equal, Text.hash);
        };
    }

}
