import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";

import Error "mo:base/Error";

import Types "../common/types";
import DecksTypes "./decks.types";
import DecksStore "./decks.store";

actor Deck {
    type DeckId = Types.DeckId;
    type DeckData = DecksTypes.DeckData;
    type Deck = DecksTypes.Deck;
    type UserDeck = DecksTypes.UserDeck;

    let store: DecksStore.Store = DecksStore.Store();

    // Preserve the application state on upgrades
    private stable var decks : [(Principal, [(DeckId, UserDeck)])] = [];

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

    public shared({ caller }) func entries() : async [Deck] {
        let decks: [Deck] = await store.getDecks(caller);
        return decks;
    };

    public shared({ caller }) func del(deckId : DeckId, slides: Bool) : async (Bool) {
        let admin : Principal = Principal.fromActor(Deck);
        
        let exists: Bool = await store.deleteDeck(admin, caller, deckId, slides);

        return exists;
    };

    system func preupgrade() {
        decks := Iter.toArray(store.preupgrade().entries());
    };

    system func postupgrade() {
        store.postupgrade(decks);
        decks := [];
    };
}
