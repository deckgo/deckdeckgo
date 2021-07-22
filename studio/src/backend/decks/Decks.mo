import Trie "mo:base/Trie";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Option "mo:base/Option";

import Error "mo:base/Error";

import Debug "mo:base/Debug";

actor Deck {

    private type DeckId = Text;

    private type DeckData = {
        name: Text;

        header: ?Text;

        owner_id: Text;
    };

    private type Deck = {
        id: DeckId;
        data: DeckData;
    };

    // TODO: Should we use Trie or Hashmap? Performance?
    private stable var decks : Trie.Trie<DeckId, Deck> = Trie.empty();

    public shared({ caller }) func set(deck : Deck): async() {
        Debug.print(debug_show(deck));

        await check_permission(caller, deck);

        decks := Trie.replace(
            decks,
            key(deck.id),
            Text.equal,
            ?deck,
        ).0;
    };

    public shared({ caller }) func get(deckId : DeckId) : async ?Deck {
        let deck: ?Deck = await getDeck(caller, deckId);

        return deck;
    };

    public shared({ caller }) func del(deckId : DeckId) : async Bool {
        let deck: ?Deck = await getDeck(caller, deckId);

        let exists: Bool = Option.isSome(deck);
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

    private func getDeck(user: Principal, deckId: Text): async ?Deck {
        let deck: ?Deck = Trie.find(decks, key(deckId), Text.equal);

        // TODO: I would rather like following but do not know how to cast value for check_permission which does not accept null
        // if (deck == null) {
        //     return null;
        // };
        //
        // await check_permission(caller, deck); <-- case deck ?

        switch deck {
            case (?deck) {
                await check_permission(user, deck);
            };
            case null {
                return null;
            }
        };

        return deck;
    };

    private func check_permission(user: Principal, deck: Deck) : async () {
        if (Principal.toText(user) != deck.data.owner_id) {
            throw Error.reject("User does not have the permission for the deck.");
        };
    };

    private func key(x : DeckId) : Trie.Key<DeckId> {
        return { hash = Text.hash(x); key = x };
    };
}
