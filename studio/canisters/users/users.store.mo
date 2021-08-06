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

    public class Store() {
        let utils: Utils.Utils = Utils.Utils();

        private var users: HashMap.HashMap<UserId, UserUser> = HashMap.HashMap<UserId, UserUser>(10, utils.isPrincipalEqual, Principal.hash);

        public func setUser(caller: UserId, user: User): async() {
            let newUserUser: UserUser = await initUser(caller, user);

            users.put(caller, newUserUser);
        };

        private func initUser(caller: UserId, user: User): async (UserUser) {
            let userUser: ?UserUser = await getUser(caller, user.userId);

            // If userUser is null, then it is a new user
            // If userUser is not null and there was no error, then it is a user to update

            return {
                owner = caller;
                user = user;
            }
        };

        public func getUser(caller: UserId, userId: UserId): async ?UserUser {
            let userUser: ?UserUser = users.get(userId);

            switch userUser {
                case (?userUser) {
                    await check_permission(caller, userUser);
                };
                case null {
                    return null;
                }
            };

            return userUser;
        };

        public func deleteUser(user: UserId, userId : UserId) : async Bool {
            let userUser: ?UserUser = await getUser(user, userId);

            let exists: Bool = Option.isSome(userUser);
            if (exists) {
                let removedUser: ?UserUser = users.remove(userId);
            };

            return exists;
        };

        private func check_permission(user: UserId, userUser: UserUser) : async () {
            if (user != userUser.owner) {
                throw Error.reject("User does not have the permission for the user.");
            };
        };

        public func preupgrade(): HashMap.HashMap<UserId, UserUser> {
            return users;
        };

        public func postupgrade(entries: [(UserId, UserUser)]) {
            users := HashMap.fromIter<UserId, UserUser>(entries.vals(), 10, utils.isPrincipalEqual, Principal.hash);
        };
    }

}
