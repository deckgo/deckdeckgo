import Principal "mo:base/Principal";

module {
    public func isPrincipalEqual(x: Principal, y: Principal): Bool { x == y };

    public func isPrincipalNotEqual(x: Principal, y: Principal): Bool { x != y };
}
