import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";

import Error "mo:base/Error";

import Types "../common/types";
import DecksTypes "./decks.types";
import DecksStore "./decks.store";

actor Deck {
    type DeckId = Types.DeckId;
    type Deck = DecksTypes.Deck;
    type OwnerDeck = DecksTypes.OwnerDeck;

    let store: DecksStore.Store = DecksStore.Store();

    // Preserve the application state on upgrades
    private stable var decks : [(Principal, [(DeckId, OwnerDeck)])] = [];

    public shared({ caller }) func set(deck: Deck) {
        await store.setDeck(caller, deck);
    };

    public shared({ caller }) func get(deckId : DeckId) : async Deck {
        let ownerDeck: ?OwnerDeck = await store.getDeck(caller, deckId);

        switch ownerDeck {
            case (?ownerDeck) {
                return ownerDeck.deck;
            };
            case null {
                throw Error.reject("Deck not found.");
            };
        };
    };

    public shared({ caller }) func entries() : async [Deck] {
        let decks: [Deck] = await entriesAdmin(caller);
        return decks;
    };

    public func entriesAdmin(user: Principal) : async [Deck] {
        let decks: [Deck] = await store.getDecks(user);
        return decks;
    };

    public shared({ caller }) func del(deckId : DeckId, slides: Bool) : async (Bool) {
        let exists: Bool = await delAdmin(caller, deckId, slides);

        return exists;
    };

    public func delAdmin(user: Principal, deckId : DeckId, slides: Bool) : async (Bool) {
        let exists: Bool = await store.deleteDeck(user, deckId, slides);

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
