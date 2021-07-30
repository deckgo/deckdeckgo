import Iter "mo:base/Iter";

import Error "mo:base/Error";

import Types "../common/types";
import DecksTypes "./decks.types";
import DecksStore "./decks.store";

actor Deck {
    type DeckId = Types.DeckId;
    type DeckData = DecksTypes.DeckData;
    type Deck = DecksTypes.Deck;
    type UserDeck = DecksTypes.UserDeck;

    var store: DecksStore.Store = DecksStore.Store();

    // Preserve the application state on upgrades
    private stable var entries : [(DeckId, UserDeck)] = [];

    public shared({ caller }) func set(deck: Deck) {
        await store.setDeck(caller, deck);
    };

    public shared({ caller }) func get(deckId : DeckId) : async Deck {
        let userDeck: ?UserDeck = await store.getDeck(caller, deckId);

        switch userDeck {
            case (?userDeck) {
                return userDeck.deck;
            };
            case null {
                throw Error.reject("Deck not found.");
            };
        };
    };

    public shared({ caller }) func del(deckId : DeckId) : async (Bool) {
        let exists: Bool = await store.deleteDeck(caller, deckId);

        return exists;
    };

    system func preupgrade() {
        entries := Iter.toArray(store.getDecks().entries());
    };

    system func postupgrade() {
        store.postupgrade(entries);
        entries := [];
    };
}
