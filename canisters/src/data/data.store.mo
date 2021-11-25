import Text "mo:base/Text";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Array "mo:base/Array";

import Result "mo:base/Result";

module {
    public class DataStore<T>() {
        private var data: HashMap.HashMap<Text, T> = HashMap.HashMap<Text, T>(10, Text.equal, Text.hash);

        public func put(key: Text, value: T) {
            data.put(key, value);
        };

        public func get(key: Text): ?T {
            return data.get(key);
        };

        public func del(key: Text): ?T {
            let entry: ?T = get(key);

            switch (entry) {
                case (?entry) {
                    data.delete(key);
                };
                case (null) {};
            };

            return entry;
        };

        public func entries(filter: ?Text): [T] {
            let entries: Iter.Iter<(Text, T)> = data.entries();

            switch (filter) {
                case null {
                    let values: Iter.Iter<T> = Iter.map(entries, func ((key: Text, value: T)) : T { value });
                    return Iter.toArray(values);
                };
                case (?filter) {
                    let keyValues: [(Text, T)] = Iter.toArray(entries);

                    let values: [T] = Array.mapFilter<(Text, T), T>(keyValues, func ((key: Text, value: T)) : ?T {
                        if (Text.startsWith(key, #text filter)) {
                            return ?value;
                        };

                        return null;
                    });


                    return values;
                };
            };
        };

        public func preupgrade(): HashMap.HashMap<Text, T> {
            return data;
        };

        public func postupgrade(stableData: [(Text, T)]) {
            data := HashMap.fromIter<Text, T>(stableData.vals(), 10, Text.equal, Text.hash);
        };
    }
}
