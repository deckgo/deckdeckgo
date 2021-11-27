import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Text "mo:base/Text";
import Time "mo:base/Time";
import Hash "mo:base/Hash";
import Array "mo:base/Array";
import Iter "mo:base/Iter";

import StorageTypes "./storage.types";

module {

    type Chunk = StorageTypes.Chunk;
    type Asset = StorageTypes.Asset;
    type Batch = StorageTypes.Batch;

    type AssetKey = StorageTypes.AssetKey;

    public class StorageStore() {

        private let BATCH_EXPIRY_NANOS = 300_000_000_000; // 300 seconds, 5 minutes

        private let batches: HashMap.HashMap<Nat, Batch> = HashMap.HashMap<Nat, Batch>(
            10, Nat.equal, Hash.hash,
        );

        private let chunks: HashMap.HashMap<Nat, Chunk> = HashMap.HashMap<Nat, Chunk>(
            10, Nat.equal, Hash.hash,
        );

        private var assets: HashMap.HashMap<Text, Asset> = HashMap.HashMap<Text, Asset>(
            10, Text.equal, Text.hash,
        );

        private var nextBatchID: Nat = 0;
        private var nextChunkID: Nat = 0;

        public func getAssetForUrl(url: Text): ({#asset: Asset; #error: Text}) {
            if (Text.size(url) == 0) {
                return #error "No url provided.";
            };

            let split: [Text] = Iter.toArray(Text.split(url, #text "?token="));
            let fullPath: Text = Text.trimStart(split[0], #char '/');
            let token: Text = split[1];

            return getAsset(fullPath, ?token);
        };

        public func getAsset(fullPath: Text, token: ?Text): ({#asset: Asset; #error: Text}) {
            let asset: ?Asset = assets.get(fullPath);

            switch (asset) {
                case (?asset) {
                    switch (asset.key.token) {
                        case (?assetToken) {
                            return getProtectedAsset(asset, assetToken, token);
                        };
                        case (null) {
                            return #asset asset;
                        };
                    };
                };
                case null {
                    return #error "No asset.";
                };
            };
        };

        public func getProtectedAsset(asset: Asset, assetToken: Text, token: ?Text): ({#asset: Asset; #error: Text}) {
            switch (token) {
                case null {
                    return #error "No token provided.";
                };
                case (?token) {
                    let compare: {#less; #equal; #greater} = Text.compare(token, assetToken);

                    switch (compare) {
                        case (#equal equal) {
                            return #asset asset;
                        };
                        case (#less less) {
                            return #error "Invalid token.";
                        };
                        case (#greater greater) {
                            return #error "Invalid token.";
                        };
                    };
                };
            };
        };

        public func deleteAsset(fullPath: Text, token: ?Text): ({#asset: Asset; #error: Text}) {
            let (result: {#asset: Asset; #error: Text;}) = getAsset(fullPath, token);

            switch (result) {
                case (#asset asset) {
                    assets.delete(fullPath);
                };
                case (#error error) {};
            };

            return result;
        };

        public func getKeys(folder: ?Text): [AssetKey] {
            let entries: Iter.Iter<(Text, Asset)> = assets.entries();
            let keys: Iter.Iter<AssetKey> = Iter.map(entries, func ((fullPath: Text, value: Asset)) : AssetKey { value.key });
            let allKeys: [AssetKey] = Iter.toArray(keys);

            switch (folder) {
                case null {
                    return allKeys;
                };
                case (?folder) {
                    let filteredKeys: [AssetKey] = Array.filter<AssetKey>(allKeys, func (key: AssetKey) : Bool { Text.equal(key.folder, folder) });
                    return filteredKeys;
                };
            };
        };

        public func createBatch(key: AssetKey) : (Nat) {
            nextBatchID := nextBatchID + 1;

            let now: Time.Time = Time.now();

            clearExpiredBatches();

            batches.put(nextBatchID, {
                key;
                expiresAt = now + BATCH_EXPIRY_NANOS;
            });

            return nextBatchID;
        };

        public func createChunk({batchId: Nat; content: [Nat8];}: Chunk) : ({#chunkId: Nat; #error: Text;}) {
            switch (batches.get(batchId)) {
                case (null) {
                    return #error "Batch not found.";
                };
                case (?batch) {
                    // Extend batch timeout
                    batches.put(batchId, {
                        key = batch.key;
                        expiresAt = Time.now() + BATCH_EXPIRY_NANOS;
                    });

                    nextChunkID := nextChunkID + 1;

                    chunks.put(nextChunkID, {
                        batchId;
                        content;
                    });

                    return #chunkId nextChunkID;
                };
            };
        };

        public func commitBatch(
            {batchId; chunkIds; headers} : {
                batchId: Nat;
                headers: [(Text, Text)];
                chunkIds: [Nat];
            },
        ) : ({error: ?Text;}) {
            let batch: ?Batch = batches.get(batchId);

            switch (batch) {
                case (null) {
                    return {error = ?"No batch to commit.";}
                };
                case (?batch) {
                    let error: {error: ?Text} = commitChunks({batchId; batch; chunkIds; headers;});
                    return error;
                };
            };
        };

        private func commitChunks(
            {batchId; batch; chunkIds; headers} : {
                batchId: Nat;
                batch: Batch;
                headers: [(Text, Text)];
                chunkIds: [Nat];
            }
        ) : ({error: ?Text;}) {
            // Test batch is not expired
            let now: Time.Time = Time.now();
            if (now > batch.expiresAt) {
                clearExpiredBatches();
                return {error = ?"Batch did not complete in time. Chunks cannot be commited."};
            };

            var contentChunks : [[Nat8]] = [];

            for (chunkId in chunkIds.vals()) {
                let chunk: ?Chunk = chunks.get(chunkId);

                switch (chunk) {
                    case (?chunk) {
                        if (Nat.notEqual(batchId, chunk.batchId)) {
                            return {error = ?"Chunk not included in the provided batch"};
                        };

                        contentChunks := Array.append<[Nat8]>(contentChunks, [chunk.content]);
                    };
                    case null {
                        return {error = ?"Chunk does not exist."};
                    };
                };
            };

            if (contentChunks.size() <= 0) {
                return {error = ?"No chunk to commit."};
            };

            var totalLength = 0;
            for (chunk in contentChunks.vals()) {
                totalLength += chunk.size();
            };

            assets.put(batch.key.fullPath, {
                key = batch.key;
                headers;
                encoding = {
                    modified = Time.now();
                    contentChunks;
                    totalLength
                };
            });

            clearBatch({batchId; chunkIds;});

            return {error = null};
        };

        private func clearBatch({batchId: Nat; chunkIds: [Nat];} : {batchId: Nat; chunkIds: [Nat];}) {
            for (chunkId in chunkIds.vals()) {
                chunks.delete(chunkId);
            };

            batches.delete(batchId);
        };

        private func clearExpiredBatches() {
            let now: Time.Time = Time.now();

            // Remove expired batches
            let batchEntries: Iter.Iter<(Nat, Batch)> = batches.entries();
            for ((batchId: Nat, batch: Batch) in batchEntries) {
                if (now > batch.expiresAt) {
                    batches.delete(batchId);
                };
            };
            
            // Remove chunk without existing batches (those we just deleted above)
            for ((chunkId: Nat, chunk: Chunk) in chunks.entries()) {
                switch (batches.get(chunk.batchId)) {
                    case (null) { 
                        chunks.delete(chunkId); 
                    };
                    case (?batch) {};
                };
            };
        };

        public func preupgrade(): HashMap.HashMap<Text, Asset> {
            return assets;
        };

        public func postupgrade(stableAssets: [(Text, Asset)]) {
            assets := HashMap.fromIter<Text, Asset>(stableAssets.vals(), 10, Text.equal, Text.hash);
        };
    }

}
