module {

    public type Chunk = {
        batchId : Nat;
        content  : [Nat8];
    };

    public type AssetEncoding = {
        modified       : Int;
        contentChunks : [[Nat8]];
        totalLength   : Nat;
        certified      : Bool;
        // TODO do we need sha256         : [Nat8]; ?
    };

    public type Asset = {
        encoding: AssetEncoding;
        contentType: Text;
    };

    public type Batch = {
        expiresAt : Int;
        token: Text;
    };

}
