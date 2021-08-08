import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Option "mo:base/Option";

import Error "mo:base/Error";

import Types "../common/types";
import DecksTypes "./decks.types";
import DecksStore "./decks.store";

actor Deck {
    type DeckId = Types.DeckId;
    type Deck = DecksTypes.Deck;
    type OwnerDeck = DecksTypes.OwnerDeck;
    type ProtectedDeck = DecksTypes.ProtectedDeck;
    type ProtectedDecks = DecksTypes.ProtectedDecks;

    let store: DecksStore.Store = DecksStore.Store();

    // Preserve the application state on upgrades
    private stable var decks : [(Principal, [(DeckId, OwnerDeck)])] = [];

    public shared({ caller }) func set(deck: Deck): async() {
        let error: ?Text = store.setDeck(caller, deck);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {};
        };
    };

    public shared query({ caller }) func get(deckId : DeckId) : async Deck {
        let ({error; deck;}): ProtectedDeck = store.getDeck(caller, deckId);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                switch (deck) {
                    case (?deck) {
                        return deck.deck;
                    };
                    case null {
                        throw Error.reject("Deck not found.");
                    };
                };
            };
        };
    };

    public shared query({ caller }) func entries() : async [Deck] {
        let ({error; decks}): ProtectedDecks = store.getDecks(caller);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                return decks;
            };
        };
    };

    public func entriesAdmin(user: Principal) : async [Deck] {
        let ({error; decks}): ProtectedDecks = store.getDecks(user);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                return decks;
            };
        };
    };

    public shared({ caller }) func del(deckId : DeckId, slides: Bool) : async (Bool) {
        let deck: ProtectedDeck = store.deleteDeck(caller, deckId, slides);

        switch (deck.error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                let exists: Bool = Option.isSome(deck.deck);
                return exists;
            };
        };
    };

    public func delAdmin(user: Principal, deckId : DeckId, slides: Bool) : async Bool {
        let deck: ProtectedDeck = store.deleteDeck(user, deckId, slides);

        switch (deck.error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                let exists: Bool = Option.isSome(deck.deck);
                return exists;
            };
        };
    };

    system func preupgrade() {
        decks := Iter.toArray(store.preupgrade().entries());
    };

    system func postupgrade() {
        store.postupgrade(decks);
        decks := [];
    };
}
