import Nat "mo:base/Nat";
import Text "mo:base/Text";
import Time "mo:base/Time";
import HashMap "mo:base/HashMap";
import Hash "mo:base/Hash";
import Iter "mo:base/Iter";
import Error "mo:base/Error";

import Types "../types/types";
import StorageTypes "./storage.types";

import Utils "../utils/utils";

import WalletUtils "../utils/wallet.utils";

actor class StorageBucket(owner: Types.UserId) = this {

    private let BATCH_EXPIRY_NANOS = 300_000_000_000;

    private type Chunk = StorageTypes.Chunk;
    private type Asset = StorageTypes.Asset;
    private type Batch = StorageTypes.Batch;

    private let walletUtils: WalletUtils.WalletUtils = WalletUtils.WalletUtils();

    private stable let user: Types.UserId = owner;

    // Preserve the application state on upgrades
    private stable var entries : [(Text, Asset)] = [];

    private let batches: HashMap.HashMap<Nat, Batch> = HashMap.HashMap<Nat, Batch>(
        0, Nat.equal, Hash.hash,
    );

    private let chunks: HashMap.HashMap<Nat, Chunk> = HashMap.HashMap<Nat, Chunk>(
        0, Nat.equal, Hash.hash,
    );

    private var assets: HashMap.HashMap<Text, Asset> = HashMap.HashMap<Text, Asset>(
        0, Text.equal, Text.hash,
    );

    private var nextBatchID: Nat = 0;
    private var nextChunkID: Nat = 0;

    /**
     * Upload
     */

    public shared({caller}) func create_batch({token: Text;}: {token: Text;}) : async ({batchId : Nat}) {
        if (Utils.isPrincipalNotEqual(caller, user)) {
            throw Error.reject("User does not have the permission to upload content.");
        };

        nextBatchID := nextBatchID + 1;

        let now: Time.Time = Time.now();

        batches.put(nextBatchID, {
            expiresAt = now + BATCH_EXPIRY_NANOS;
            token = token;
        });

        return {batchId = nextBatchID};
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
        entries := Iter.toArray(assets.entries());
    };

    system func postupgrade() {
        assets := HashMap.fromIter<Text, Asset>(entries.vals(), 10, Text.equal, Text.hash);
        entries := [];
    };
}