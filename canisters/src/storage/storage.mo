import Nat "mo:base/Nat";
import Text "mo:base/Text";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Error "mo:base/Error";

import Types "../types/types";
import StorageTypes "./storage.types";

import Utils "../utils/utils";

import StorageStore "./storage.store";

import WalletUtils "../utils/wallet.utils";

actor class StorageBucket(owner: Types.UserId) = this {

    private let BATCH_EXPIRY_NANOS = 300_000_000_000;

    private type Asset = StorageTypes.Asset;
    private type Chunk = StorageTypes.Chunk;

    private let walletUtils: WalletUtils.WalletUtils = WalletUtils.WalletUtils();

    private stable let user: Types.UserId = owner;

    // Preserve the application state on upgrades
    private stable var entries : [(Text, Asset)] = [];

    let storageStore: StorageStore.StorageStore = StorageStore.StorageStore();

    /**
     * Upload
     */

    public shared({caller}) func create_batch({path: Text; token: Text;}: {path: Text; token: Text;}) : async ({batchId : Nat}) {
        if (Utils.isPrincipalNotEqual(caller, user)) {
            throw Error.reject("User does not have the permission to create a batch for upload.");
        };

        let nextBatchID: Nat = storageStore.createBatch(path, token);

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
