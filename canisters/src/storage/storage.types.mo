module {

    public type Chunk = {
        batchId: Nat;
        content: [Nat8];
    };

    public type AssetEncoding = {
        modified: Int;
        contentChunks: [[Nat8]];
        totalLength: Nat;
    };

    public type AssetKey = {
        name: Text; // myimage.jpg
        folder: Text; // images
        fullPath: Text; // /images/myimage.jpg
        token: ?Text; // ?token=1223-3345-5564-3333
    };

    public type Asset = {
        key: AssetKey;
        headers: [(Text, Text)];
        encoding: AssetEncoding;
    };

    public type Batch = {
        key: AssetKey;
        expiresAt: Int;
    };

}
