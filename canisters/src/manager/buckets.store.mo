import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Iter "mo:base/Iter";
import Array "mo:base/Array";

import Types "../types/types";
import CanisterTypes "../types/canister.types";

import Utils "../utils/utils";

import CanisterUtils "../utils/canister.utils";

module {
    type UserId = Types.UserId;

    type BucketId = CanisterTypes.BucketId;

    public class BucketsStore<T>() {
        private var buckets: HashMap.HashMap<UserId, CanisterTypes.Bucket<T>> = HashMap.HashMap<UserId, CanisterTypes.Bucket<T>>(10, Utils.isPrincipalEqual, Principal.hash);

        private let canisterUtils: CanisterUtils.CanisterUtils = CanisterUtils.CanisterUtils();

        public func init(manager: Principal, user: UserId, initNewBucket: (manager: Principal, user: UserId, buckets: HashMap.HashMap<UserId, CanisterTypes.Bucket<T>>) -> async (Principal)): async ({#bucketId: BucketId; #error: Text;}) {
            let bucket: {#bucketId: ?BucketId; #error: Text;} = getBucket(user);

            switch (bucket) {
                case (#error error) {
                    return #error error;
                };
                case (#bucketId bucketId) {
                    switch (bucketId) {
                        case (?bucketId) {
                            return #bucketId bucketId;
                        };
                        case null {
                            let newBucketId: Principal = await initNewBucket(manager, user, buckets);

                            return #bucketId newBucketId;
                        };
                    }
                };
            };
        };

        public func getBucket(user: UserId): {#bucketId: ?BucketId; #error: Text;} {
            let bucket: ?CanisterTypes.Bucket<T> = buckets.get(user);

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

            return #error "User does not have the permission for the bucket.";
        };

        public func deleteBucket(user: UserId) : async ({#bucketId: ?BucketId; #error: Text;}) {
            let bucket: {#bucketId: ?BucketId; #error: Text;} = getBucket(user);

            switch (bucket) {
                case (#error error) {
                    return #error error;
                };
                case (#bucketId bucketId) {
                    switch (bucketId) {
                        case (?bucketId) {
                            await canisterUtils.deleteCanister(bucketId);

                            buckets.delete(user);
                        };
                        case null {};
                    };

                    return #bucketId bucketId;
                };
            };
        };

        public func preupgrade(): HashMap.HashMap<UserId, CanisterTypes.Bucket<T>> {
            return buckets;
        };

        public func postupgrade(stableBuckets: [(UserId, CanisterTypes.Bucket<T>)]) {
            buckets := HashMap.fromIter<UserId, CanisterTypes.Bucket<T>>(stableBuckets.vals(), 10, Utils.isPrincipalEqual, Principal.hash);
        };
    }

}
