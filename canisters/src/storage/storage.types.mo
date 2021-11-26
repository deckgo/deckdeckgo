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

    public type AssetKey = {
        name: Text; // myimage.jpg
        folder: Text; // images
        fullPath: Text; // /images/myimage.jpg
        token: ?Text; // ?token=1223-3345-5564-3333
    };

    public type Asset = {
        key: AssetKey;
        contentType: Text;
        encoding: AssetEncoding;
    };

    public type Batch = {
        key: AssetKey;
        expiresAt: Int;
    };

}
