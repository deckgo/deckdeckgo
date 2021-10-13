import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Cycles "mo:base/ExperimentalCycles";

import Error "mo:base/Error";

import Types "../types/types";
import CanisterTypes "../types/canister.types";

import Utils "../utils/utils";

import StorageBucket "../storage/storage";

import CanisterUtils "../utils/canister.utils";

module {
    type UserId = Types.UserId;

    type StorageBucket = StorageBucket.StorageBucket;

    type BucketId = CanisterTypes.BucketId;
    type OwnerStorageBucket = CanisterTypes.Bucket<StorageBucket>;

    public class StoragesStore() {
        private var storages: HashMap.HashMap<UserId, OwnerStorageBucket> = HashMap.HashMap<UserId, OwnerStorageBucket>(10, Utils.isPrincipalEqual, Principal.hash);

        private let canisterUtils: CanisterUtils.CanisterUtils = CanisterUtils.CanisterUtils();

        public func init(manager: Principal, user: UserId): async ({#bucketId: BucketId; #error: Text;}) {
            let storageBucket: {#bucketId: ?BucketId; #error: Text;} = getStorage(user);

            switch (storageBucket) {
                case (#error error) {
                    return #error error;
                };
                case (#bucketId bucketId) {
                    switch (bucketId) {
                        case (?bucketId) {
                            return #bucketId bucketId;
                        };
                        case null {
                            let b: StorageBucket = await initNewBucket(manager, user);

                            let newBucketId: Principal = Principal.fromActor(b);

                            let newStorageBucket: OwnerStorageBucket = {
                                bucket = b;
                                bucketId = newBucketId;
                                owner = user;
                            };

                            storages.put(user, newStorageBucket);

                            return #bucketId newBucketId;
                        };
                    }
                };
            };
        };

        private func initNewBucket(manager: Principal, user: UserId): async (StorageBucket) {
            Cycles.add(1_000_000_000_000);
            let b: StorageBucket = await StorageBucket.StorageBucket(user);

            let canisterId: Principal = Principal.fromActor(b);

            await canisterUtils.updateSettings(canisterId, manager, user);

            return b;
        };

        public func getStorage(user: UserId): {#bucketId: ?BucketId; #error: Text;} {
            let bucket: ?OwnerStorageBucket = storages.get(user);

            switch bucket {
                case (?bucket) {
                    if (Utils.isPrincipalEqual(user, bucket.owner)) {
                        let bucketId: ?BucketId = ?bucket.bucketId;
                        return #bucketId bucketId;
                    };
                };
                case null {
                    return #bucketId null;
                };
            };

            return #error "User does not have the permission for the storage.";
        };

        public func deleteStorage(user: UserId) : async ({#bucketId: ?BucketId; #error: Text;}) {
            let bucket: {#bucketId: ?BucketId; #error: Text;} = getStorage(user);

            switch (bucket) {
                case (#error error) {
                    return #error error;
                };
                case (#bucketId bucketId) {
                    switch (bucketId) {
                        case (?bucketId) {
                            await canisterUtils.deleteCanister(bucketId);

                            storages.delete(user);
                        };
                        case null {};
                    };

                    return #bucketId bucketId;
                };
            };
        };

        public func preupgrade(): HashMap.HashMap<UserId, OwnerStorageBucket> {
            return storages;
        };

        public func postupgrade(stableStorages: [(UserId, OwnerStorageBucket)]) {
            storages := HashMap.fromIter<UserId, OwnerStorageBucket>(stableStorages.vals(), 10, Utils.isPrincipalEqual, Principal.hash);
        };
    }

}
