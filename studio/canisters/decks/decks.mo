import Trie "mo:base/Trie";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Option "mo:base/Option";

import Error "mo:base/Error";

actor Deck {

    private type DeckId = Text;

    private type DeckData = {
        name: Text;
        header: ?Text;
    };

    private type Deck = {
        id: DeckId;
        data: DeckData;
    };

    private type UserDeck = {
        owner: Principal;
        deck: Deck;
    };

    // TODO: Should we use Trie or Hashmap? Performance?
    private stable var decks : Trie.Trie<DeckId, UserDeck> = Trie.empty();

    public shared({ caller }) func set(deck : Deck): async() {
        let newUserDeck: UserDeck = await init(caller, deck);

        decks := Trie.replace(
            decks,
            key(deck.id),
            Text.equal,
            ?newUserDeck,
        ).0;
    };

    public shared({ caller }) func get(deckId : DeckId) : async Deck {
        let userDeck: ?UserDeck = await getDeck(caller, deckId);

        switch userDeck {
            case (?userDeck) {
                return userDeck.deck;
            };
            case null {
                throw Error.reject("Deck not found.");
            };
        };
    };

    public shared({ caller }) func del(deckId : DeckId) : async Bool {
        let userDeck: ?UserDeck = await getDeck(caller, deckId);

        let exists: Bool = Option.isSome(userDeck);
        if (exists) {
            decks := Trie.replace(
                decks,
                key(deckId),
                Text.equal,
                null,
            ).0;
        };
        return exists;
    };

    private func getDeck(user: Principal, deckId: Text): async ?UserDeck {
        let userDeck: ?UserDeck = Trie.find(decks, key(deckId), Text.equal);

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

    private func check_permission(user: Principal, userDeck: UserDeck) : async () {
        if (user != userDeck.owner) {
            throw Error.reject("User does not have the permission for the deck.");
        };
    };

    private func key(x : DeckId) : Trie.Key<DeckId> {
        return { hash = Text.hash(x); key = x };
    };

    private func init(user: Principal, deck: Deck): async (UserDeck) {
        let userDeck: ?UserDeck = await getDeck(user, deck.id);

        // If userDeck is null, then it is a new deck
        // If userDeck is not null and there was no error, then it is user deck

        return {
            owner = user;
            deck = deck;
        }
    }
}
