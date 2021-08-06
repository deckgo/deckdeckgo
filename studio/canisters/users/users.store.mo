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

    public class Store() {
        private var users: HashMap.HashMap<UserId, OwnerUser> = HashMap.HashMap<UserId, OwnerUser>(10, Utils.isPrincipalEqual, Principal.hash);

        public func setUser(caller: UserId, user: User): ?Text {
            let ownerUser: ProtectedUser = getUser(caller, user.userId);

            switch (ownerUser.error) {
                case (?error) {
                    return ?error;
                };
                case null {
                    users.put(caller, {
                        owner = caller;
                        user = user;
                    });
                };
            };

            return null;
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

        public func preupgrade(): HashMap.HashMap<UserId, OwnerUser> {
            return users;
        };

        public func postupgrade(entries: [(UserId, OwnerUser)]) {
            users := HashMap.fromIter<UserId, OwnerUser>(entries.vals(), 10, Utils.isPrincipalEqual, Principal.hash);
        };
    }

}
