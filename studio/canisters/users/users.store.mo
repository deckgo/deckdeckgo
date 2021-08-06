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
    type UserUser = UsersTypes.UserUser;
    type ProtectedUser = UsersTypes.ProtectedUser;

    public class Store() {
        private var users: HashMap.HashMap<UserId, UserUser> = HashMap.HashMap<UserId, UserUser>(10, Utils.isPrincipalEqual, Principal.hash);

        public func setUser(caller: UserId, user: User): ?Text {
            let userUser: ProtectedUser = getUser(caller, user.userId);

            switch (userUser.error) {
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
            let userUser: ?UserUser = users.get(userId);

            switch userUser {
                case (?userUser) {
                    if (Utils.isPrincipalEqual(caller, userUser.owner)) {
                        return {
                            user = ?userUser;
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
            let userUser: ProtectedUser = getUser(caller, userId);

            let exists: Bool = Option.isSome(userUser.user);
            if (exists) {
                let removedUser: ?UserUser = users.remove(userId);
            };

            return userUser;
        };

        public func preupgrade(): HashMap.HashMap<UserId, UserUser> {
            return users;
        };

        public func postupgrade(entries: [(UserId, UserUser)]) {
            users := HashMap.fromIter<UserId, UserUser>(entries.vals(), 10, Utils.isPrincipalEqual, Principal.hash);
        };
    }

}
