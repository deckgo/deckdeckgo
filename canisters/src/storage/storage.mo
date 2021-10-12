import Nat "mo:base/Nat";
import Text "mo:base/Text";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Error "mo:base/Error";
import Blob "mo:base/Blob";
import Principal "mo:base/Principal";

import Types "../types/types";
import HTTP "../types/http";
import StorageTypes "./storage.types";

import Utils "../utils/utils";

import StorageStore "./storage.store";

import WalletUtils "../utils/wallet.utils";

actor class StorageBucket(owner: Types.UserId) = this {

    private let BATCH_EXPIRY_NANOS = 300_000_000_000;

    private type Asset = StorageTypes.Asset;
    private type AssetEncoding = StorageTypes.AssetEncoding;
    private type Chunk = StorageTypes.Chunk;

    private type HttpRequest = HTTP.HttpRequest;
    private type HttpResponse = HTTP.HttpResponse;
    private type StreamingCallbackHttpResponse = HTTP.StreamingCallbackHttpResponse;
    private type StreamingCallbackToken = HTTP.StreamingCallbackToken;
    private type StreamingStrategy = HTTP.StreamingStrategy;

    private let walletUtils: WalletUtils.WalletUtils = WalletUtils.WalletUtils();

    private stable let user: Types.UserId = owner;

    // Preserve the application state on upgrades
    private stable var entries : [(Text, Asset)] = [];

    let storageStore: StorageStore.StorageStore = StorageStore.StorageStore();

    /**
     * HTTP
     */

    public shared query({caller}) func http_request({method: Text; url: Text;} : HttpRequest) : async HttpResponse {
        try {
            if (Text.notEqual(method, "GET")) {
                return {
                    body = Blob.toArray(Text.encodeUtf8("Method Not Allowed."));
                    headers = [];
                    status_code = 405;
                    streaming_strategy = null;
                };
            };

            let (result: {#asset: Asset; #error: Text;}) = storageStore.getAssetForUrl(url);

            switch (result) {
                case (#asset {name: Text; fullPath: Text; token: Text; contentType: Text; encoding: AssetEncoding;}) {
                    return {
                        body = encoding.contentChunks[0];
                        headers = [ ("Content-Type", contentType),
                                    ("accept-ranges", "bytes"),
                                    ("cache-control", "private, max-age=0") ];
                        status_code = 200;
                        streaming_strategy = createStrategy(name, fullPath, token, 0, encoding);
                    };
                };
                case (#error error) {
                };
            };

            return {
                body = Blob.toArray(Text.encodeUtf8("Permission denied. Could not perform this operation."));
                headers = [];
                status_code = 403;
                streaming_strategy = null;
            };
        } catch (err) {
            return {
                body = Blob.toArray(Text.encodeUtf8("Unexpected error: " # Error.message(err)));
                headers = [];
                status_code = 500;
                streaming_strategy = null;
            };
        }
    };

    public shared query({caller}) func http_request_streaming_callback(streamingToken: StreamingCallbackToken) : async StreamingCallbackHttpResponse {
        let (result: {#asset: Asset; #error: Text;}) = storageStore.getAsset(streamingToken.fullPath, streamingToken.token);

        switch (result) {
            case (#asset {name: Text; fullPath: Text; token: Text; contentType: Text; encoding: AssetEncoding;}) {
                return {
                    token = createToken(name, fullPath, token, streamingToken.index, encoding);
                    body = encoding.contentChunks[streamingToken.index];
                };
            };
            case (#error error) {
                throw Error.reject("Streamed asset not found: " # error);
            };
        };
    };

    private func createStrategy(name: Text, fullPath: Text, token: Text, index: Nat, encoding: AssetEncoding) : ?StreamingStrategy {
        let streamingToken: ?StreamingCallbackToken = createToken(name, fullPath, token, index, encoding);

        switch (streamingToken) {
            case (null) { null };
            case (?streamingToken) {
                // Hack: https://forum.dfinity.org/t/cryptic-error-from-icx-proxy/6944/8
                // Issue: https://github.com/dfinity/candid/issues/273

                let self: Principal = Principal.fromActor(this);
                let canisterId: Text = Principal.toText(self);

                let canister = actor (canisterId) : actor { http_request_streaming_callback : shared () -> async () };

                return ?#Callback({
                    token = streamingToken;
                    callback = canister.http_request_streaming_callback;
                });
            };
        };
    };

    private func createToken(name: Text, fullPath: Text, token: Text, chunkIndex: Nat, encoding: AssetEncoding) : ?StreamingCallbackToken {
        // TODO encoding and sha
        // TODO always gzip?

        if (chunkIndex + 1 >= encoding.contentChunks.size()) {
            return null;
        };
        
        let streamingToken: ?StreamingCallbackToken = ?{
            name;
            fullPath;
            token;
            index = chunkIndex + 1;
            contentEncoding = "gzip";
        };

        return streamingToken;
    };

    /**
     * Upload
     */

    public shared({caller}) func create_batch({name: Text; fullPath: Text; token: Text;}: {name: Text; fullPath: Text; token: Text;}) : async ({batchId : Nat}) {
        if (Utils.isPrincipalNotEqual(caller, user)) {
            throw Error.reject("User does not have the permission to create a batch for upload.");
        };

        let nextBatchID: Nat = storageStore.createBatch(name, fullPath, token);

        return {batchId = nextBatchID};
    };

    public shared({caller}) func create_chunk(chunk: Chunk) : async ({chunkId : Nat}) {
        if (Utils.isPrincipalNotEqual(caller, user)) {
            throw Error.reject("User does not have the permission to a upload a chunk of content.");
        };

        let (result: {#chunkId: Nat; #error: Text;}) = storageStore.createChunk(chunk);

        switch (result) {
            case (#error error) {
                throw Error.reject(error);
            };
            case (#chunkId chunkId) {
                return {chunkId};
            };
        };
    };

    public shared({caller}) func commit_batch(
        {batchId: Nat; chunkIds: [Nat]; contentType: Text;} : {
            batchId: Nat;
            contentType: Text;
            chunkIds: [Nat];
        },
    ) : async () {
        if (Utils.isPrincipalNotEqual(caller, user)) {
            throw Error.reject("User does not have the permission to commit a batch.");
        };

        let ({error}: {error: ?Text;}) = storageStore.commitBatch({batchId; contentType; chunkIds;});

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {};
        };
    };

    /**
     * List
     */

    public shared query({ caller }) func list() : async [{name: Text; fullPath: Text; token: Text;}] {
        if (Utils.isPrincipalNotEqual(caller, user)) {
            throw Error.reject("User does not have the permission to list the assets.");
        };

        let keys: [{name: Text; fullPath: Text; token: Text;}] = storageStore.getKeys();
        return keys;
    };

    /**
    * Canister mgmt
    */

    // TODO: inter-canister call secure caller === manager canister !!!
    // Or as only controllers can execute following is enough security?

    public shared({ caller }) func transferCycles(): async() {
        await walletUtils.transferCycles(caller);
    };

    system func preupgrade() {
        entries := Iter.toArray(storageStore.preupgrade().entries());
    };

    system func postupgrade() {
        storageStore.postupgrade(entries);
        entries := [];
    };
}
