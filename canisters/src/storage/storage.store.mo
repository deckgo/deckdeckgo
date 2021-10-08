import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Text "mo:base/Text";
import Time "mo:base/Time";
import Hash "mo:base/Hash";

import StorageTypes "./storage.types";

module {

    type Chunk = StorageTypes.Chunk;
    type Asset = StorageTypes.Asset;
    type Batch = StorageTypes.Batch;

    public class StorageStore() {

        private let BATCH_EXPIRY_NANOS = 300_000_000_000;

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

        public func createBatch(token: Text) : (Nat) {
            nextBatchID := nextBatchID + 1;

            let now: Time.Time = Time.now();

            batches.put(nextBatchID, {
                expiresAt = now + BATCH_EXPIRY_NANOS;
                token = token;
            });

            return nextBatchID;
        };

        public func preupgrade(): HashMap.HashMap<Text, Asset> {
            return assets;
        };

        public func postupgrade(stableAssets: [(Text, Asset)]) {
            assets := HashMap.fromIter<Text, Asset>(stableAssets.vals(), 10, Text.equal, Text.hash);
        };
    }

}