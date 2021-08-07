import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";

import Error "mo:base/Error";

import Types "../common/types";
import UsersTypes "./users.types";

import Utils "../common/utils";

module {
    type UserId = Types.UserId;
    type User = UsersTypes.User;
    type OwnerUser = UsersTypes.OwnerUser;
    type ProtectedUser = UsersTypes.ProtectedUser;
    type Username = UsersTypes.Username;

    public class Store() {
        private var users: HashMap.HashMap<UserId, OwnerUser> = HashMap.HashMap<UserId, OwnerUser>(10, Utils.isPrincipalEqual, Principal.hash);

        private var usernames: HashMap.HashMap<Username, UserId> = HashMap.HashMap<Username, UserId>(10, Text.equal, Text.hash);

        public func setUser(caller: UserId, user: User): ?Text {
            let ({error}): ProtectedUser = getUser(caller, user.userId);

            switch (error) {
                case (?error) {
                    return ?error;
                };
                case null {
                    let valid: Bool = validUsername(caller, user);

                    if (valid == false) {
                        return ?"Username already exists";
                    };

                    users.put(caller, {
                        owner = caller;
                        user = user;
                    });

                    setUsername(caller, user);
                };
            };

            return null;
        };

        private func validUsername(caller: UserId, user: User): Bool {
            let username: ?Username = user.data.username;

            switch (username) {
                case (?username) {
                    let userId: ?UserId = usernames.get(username);

                    switch (userId) {
                        case (?userId) {
                            if (userId != caller) {
                                return false;
                            }
                        };
                        case null {};
                    }
                };
                case (null) {};
            };

            return true;
        };

        private func setUsername(caller: UserId, user: User) {
            let newUsername: ?Username = user.data.username;

            switch (newUsername) {
                case (?newUsername) {
                    usernames.put(newUsername, caller);
                };
                case null {};
            }
        };

        public func getUser(caller: UserId, userId: UserId): ProtectedUser {
            let ownerUser: ?OwnerUser = users.get(userId);

            switch ownerUser {
                case (?ownerUser) {
                    if (Utils.isPrincipalEqual(caller, ownerUser.owner)) {
                        return {
                            user = ?ownerUser;
                            error = null;
                        };
                    };
                };
                case null {
                    return {
                        user = null;
                        error = null;
                    }
                };
            };

            return {
                user = null;
                error = ?"User does not have the permission for the user.";
            }
        };

        public func deleteUser(caller: UserId, userId : UserId) : ProtectedUser {
            let ownerUser: ProtectedUser = getUser(caller, userId);

            let exists: Bool = Option.isSome(ownerUser.user);
            if (exists) {
                let removedUser: ?OwnerUser = users.remove(userId);
            };

            return ownerUser;
        };

        public func preupgrade(): {users: HashMap.HashMap<UserId, OwnerUser>; usernames: HashMap.HashMap<Username, UserId>;} {
            return {
                users = users;
                usernames = usernames;
            };
        };

        public func postupgrade(stableUsers: [(UserId, OwnerUser)], stableUsernames: [(Username, UserId)]) {
            users := HashMap.fromIter<UserId, OwnerUser>(stableUsers.vals(), 10, Utils.isPrincipalEqual, Principal.hash);

            usernames := HashMap.fromIter<Username, UserId>(stableUsernames.vals(), 10, Text.equal, Text.hash);
        };
    }

}
