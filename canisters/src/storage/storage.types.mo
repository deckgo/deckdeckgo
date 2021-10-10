module {

    public type Chunk = {
        batchId: Nat;
        content: [Nat8];
    };

    public type AssetEncoding = {
        modified: Int;
        contentChunks: [[Nat8]];
        totalLength: Nat;
        // TODO: certified: Bool; should we use the certified flag?
        // TODO: do we need sha256         : [Nat8]; ?
    };

    public type Asset = {
        path: Text;
        token: Text;
        contentType: Text;
        encoding: AssetEncoding;
    };

    public type Batch = {
        path: Text; // /images/myimage.jpg
        token: Text; // ?token=1223-3345-5564-3333
        expiresAt: Int;
    };

}
