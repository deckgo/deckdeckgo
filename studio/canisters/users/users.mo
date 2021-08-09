import Iter "mo:base/Iter";
import Option "mo:base/Option";
import Principal "mo:base/Principal";

import Error "mo:base/Error";

import Types "../common/types";
import UsersTypes "./users.types";
import UsersStore "./users.store";
import DecksTypes "../decks/decks.types";

import Decks "canister:decks";

actor User {
    type UserId = Types.UserId;
    type User = UsersTypes.User;
    type OwnerUser = UsersTypes.OwnerUser;
    type ProtectedUser = UsersTypes.ProtectedUser;
    type Username = UsersTypes.Username;

    type Deck = DecksTypes.Deck;
    type DeckId = Types.DeckId;
    type DeckData = DecksTypes.DeckData;

    var store: UsersStore.Store = UsersStore.Store();

    // Preserve the application state on upgrades
    private stable var users : [(UserId, OwnerUser)] = [];
    private stable var usernames : [(Username, UserId)] = [];

    public shared({ caller }) func set(user: User): async () {
        let error: ?Text = store.setUser(caller, user);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {};
        };
    };

    public shared query({ caller }) func get(userId: UserId) : async ?User {
        let ({error; user}): ProtectedUser = store.getUser(caller, userId);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                switch (user) {
                    case (?user) {
                        return ?user.user;
                    };
                    case null {
                        // We do not throw a "Not found error" here.
                        // It is a special case of the user because if not found, the very first sign in, we create a blank user.
                        // Use case developed in the web app.
                        return null;
                    };
                };
            };
        };
    };

    public shared query({ caller }) func getUserId(): async UserId {
        return caller;
    };

    public shared({ caller }) func del(userId: UserId) : async (Bool) {
        await deleteDecks(caller, userId);

        let user: ProtectedUser = store.deleteUser(caller, userId);

        switch (user.error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                let exists: Bool = Option.isSome(user.user);
                return exists;
            };
        };
    };

    private func deleteDecks(user: Principal, userId: UserId): async () {
        let decks: ([Deck]) = await Decks.entriesAdmin(userId);

        for ((deck: Deck) in decks.vals()) {
            let exists: Bool = await Decks.delAdmin(user, deck.deckId, true);
        };
    };

    system func preupgrade() {
        let ({users = activeUsers; usernames = activeUsernames;}) = store.preupgrade();
        users := Iter.toArray(activeUsers.entries());
        usernames := Iter.toArray(activeUsernames.entries());
    };

    system func postupgrade() {
        store.postupgrade(users, usernames);
        users := [];
    };
}
