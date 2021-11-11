import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Iter "mo:base/Iter";
import Array "mo:base/Array";

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

    public class StoragesStore<T>() {
        private var storages: HashMap.HashMap<UserId, CanisterTypes.Bucket<T>> = HashMap.HashMap<UserId, CanisterTypes.Bucket<T>>(10, Utils.isPrincipalEqual, Principal.hash);

        private let canisterUtils: CanisterUtils.CanisterUtils = CanisterUtils.CanisterUtils();

        public func init(manager: Principal, user: UserId, initNewBucket: (manager: Principal, user: UserId, storages: HashMap.HashMap<UserId, CanisterTypes.Bucket<T>>) -> async (Principal)): async ({#bucketId: BucketId; #error: Text;}) {
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
                            let newBucketId: Principal = await initNewBucket(manager, user, storages);

                            return #bucketId newBucketId;
                        };
                    }
                };
            };
        };

        public func getStorage(user: UserId): {#bucketId: ?BucketId; #error: Text;} {
            let bucket: ?CanisterTypes.Bucket<T> = storages.get(user);

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

        public func preupgrade(): HashMap.HashMap<UserId, CanisterTypes.Bucket<T>> {
            return storages;
        };

        public func postupgrade(stableStorages: [(UserId, CanisterTypes.Bucket<T>)]) {
            storages := HashMap.fromIter<UserId, CanisterTypes.Bucket<T>>(stableStorages.vals(), 10, Utils.isPrincipalEqual, Principal.hash);
        };
    }

}
