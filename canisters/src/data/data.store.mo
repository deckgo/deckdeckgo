import Text "mo:base/Text";
import HashMap "mo:base/HashMap";

import Result "mo:base/Result";

module {
    public class DataStore<T>() {
        private var data: HashMap.HashMap<Text, T> = HashMap.HashMap<Text, T>(10, Text.equal, Text.hash);

        public func put(key: Text, obj: T) {
            data.put(key, obj);
        };

        public func get(key: Text): Result.Result<T, Text> {
            let obj: ?T = data.get(key);

            switch (obj) {
                case (?obj) {
                    return #ok obj;
                };
                case null {
                    return #err "Not found.";
                };
            };
        };

        public func del(key: Text): Result.Result<T, Text> {
            let result: Result.Result<T, Text> = get(key);

            switch (result) {
                case (#ok obj) {
                    data.delete(key);
                };
                case (#err error) {};
            };

            return result;
        };

        public func preupgrade(): HashMap.HashMap<Text, T> {
            return data;
        };

        public func postupgrade(stableData: [(Text, T)]) {
            data := HashMap.fromIter<Text, T>(stableData.vals(), 10, Text.equal, Text.hash);
        };
    }
}
