import Iter "mo:base/Iter";

import Error "mo:base/Error";

import Types "../common/types";
import UsersTypes "./users.types";
import UsersStore "./users.store";

actor User {
    type UserId = Types.UserId;
    type User = UsersTypes.User;
    type UserUser = UsersTypes.UserUser;

    var store: UsersStore.Store = UsersStore.Store();

    // Preserve the application state on upgrades
    private stable var users : [(UserId, UserUser)] = [];

    public shared({ caller }) func set(user: User) {
        await store.setUser(caller, user);
    };

    public shared({ caller }) func get(userId: UserId) : async User {
        let user: ?UserUser = await store.getUser(caller, userId);

        switch user {
            case (?user) {
                return user.user;
            };
            case null {
                throw Error.reject("User not found.");
            };
        };
    };

    public shared query({ caller }) func getUserId(): async UserId { 
        return caller;
    };

    public shared({ caller }) func del(userId: UserId) : async (Bool) {
        let exists: Bool = await store.deleteUser(caller, userId);

        return exists;
    };

    system func preupgrade() {
        users := Iter.toArray(store.preupgrade().entries());
    };

    system func postupgrade() {
        store.postupgrade(users);
        users := [];
    };
}
