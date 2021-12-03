import Text "mo:base/Text";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Array "mo:base/Array";

import Filter "./data.filter";

module {
    type DataFilter = Filter.DataFilter;

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

        public func entries(filter: ?DataFilter): [(Text, T)] {
            let entries: Iter.Iter<(Text, T)> = data.entries();

            switch (filter) {
                case null {
                    return Iter.toArray(entries);
                };
                case (?filter) {
                    let keyValues: [(Text, T)] = Iter.toArray(entries);

                    let {startsWith; notContains} = filter;

                    let values: [(Text, T)] = Array.mapFilter<(Text, T), (Text, T)>(keyValues, func ((key: Text, value: T)) : ?(Text, T) {
                        if (Filter.startsWith(key, startsWith) and Filter.notContains(key, notContains)) {
                            return ?(key, value);
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
