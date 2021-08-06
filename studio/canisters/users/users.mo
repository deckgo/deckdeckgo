import Iter "mo:base/Iter";
import Option "mo:base/Option";

import Error "mo:base/Error";

import Types "../common/types";
import UsersTypes "./users.types";
import UsersStore "./users.store";

actor User {
    type UserId = Types.UserId;
    type User = UsersTypes.User;
    type OwnerUser = UsersTypes.OwnerUser;
    type ProtectedUser = UsersTypes.ProtectedUser;

    var store: UsersStore.Store = UsersStore.Store();

    // Preserve the application state on upgrades
    private stable var users : [(UserId, OwnerUser)] = [];

    public shared({ caller }) func set(user: User) {
        let error: ?Text = store.setUser(caller, user);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {};
        };
    };

    public shared query({ caller }) func get(userId: UserId) : async User {
        let ({error; user}): ProtectedUser = store.getUser(caller, userId);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                switch (user) {
                    case (?user) {
                        return user.user;
                    };
                    case null {
                        throw Error.reject("User not found.");
                    };
                };
            };
        };
    };

    public shared query({ caller }) func getUserId(): async UserId {
        return caller;
    };

    public shared({ caller }) func del(userId: UserId) : async (Bool) {
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

    system func preupgrade() {
        users := Iter.toArray(store.preupgrade().entries());
    };

    system func postupgrade() {
        store.postupgrade(users);
        users := [];
    };
}
