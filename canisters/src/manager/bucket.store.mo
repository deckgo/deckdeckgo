import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Iter "mo:base/Iter";
import Array "mo:base/Array";

import Types "../types/types";
import BucketTypes "./bucket.types";

import Utils "../utils/utils";
import CanisterUtils "../utils/canister.utils";

module {
    private type UserId = Types.UserId;

    private type BucketId = BucketTypes.BucketId;

    public class BucketStore() {
        private var buckets: HashMap.HashMap<UserId, BucketTypes.Bucket> = HashMap.HashMap<UserId, BucketTypes.Bucket>(10, Utils.isPrincipalEqual, Principal.hash);

        private let canisterUtils: CanisterUtils.CanisterUtils = CanisterUtils.CanisterUtils();

        public func init(manager: Principal, user: UserId, initNewBucket: (manager: Principal, user: UserId, buckets: HashMap.HashMap<UserId, BucketTypes.Bucket>) -> async (Principal)): async ({#bucketId: BucketId; #error: Text;}) {
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
            let bucket: ?BucketTypes.Bucket = buckets.get(user);

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

        public func entries(): [{bucketId: BucketId; owner: UserId;}] {
            let entries: Iter.Iter<(UserId, BucketTypes.Bucket)> = buckets.entries();
            let values: Iter.Iter<{bucketId: BucketId; owner: UserId;}> = Iter.map(entries, func ((key: UserId, value: BucketTypes.Bucket)) : {bucketId: BucketId; owner: UserId;} {
                {
                    bucketId = value.bucketId;
                    owner = value.owner;
                };
             });
            return Iter.toArray(values);
        };

        public func preupgrade(): HashMap.HashMap<UserId, BucketTypes.Bucket> {
            return buckets;
        };

        public func postupgrade(stableBuckets: [(UserId, BucketTypes.Bucket)]) {
            buckets := HashMap.fromIter<UserId, BucketTypes.Bucket>(stableBuckets.vals(), 10, Utils.isPrincipalEqual, Principal.hash);
        };
    }

}
