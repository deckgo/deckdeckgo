import Text "mo:base/Text";

module {

    public type DataFilter = {
        startsWith: ?Text;
        notContains: ?Text;
    };

    public func startsWith(key: Text, startsWith: ?Text): Bool {
        switch (startsWith) {
            case null {
                return true;
            };
            case (?startsWith) {
                return Text.startsWith(key, #text startsWith);
            };
        };
    };

    public func contains(key: Text, contains: ?Text): Bool {
        switch (contains) {
            case null {
                return true;
            };
            case (?contains) {
                return Text.contains(key, #text contains);
            };
        };
    };

}
