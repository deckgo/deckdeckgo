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

    public func notContains(key: Text, notContains: ?Text): Bool {
        switch (notContains) {
            case null {
                return true;
            };
            case (?notContains) {
                return not Text.contains(key, #text notContains);
            };
        };
    };

}
