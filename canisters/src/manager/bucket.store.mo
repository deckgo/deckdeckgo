import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Result "mo:base/Result";

import Types "../types/types";
import BucketTypes "./bucket.types";

import Utils "../utils/utils";
import CanisterUtils "../utils/canister.utils";

module {
    private type UserId = Types.UserId;

    private type Bucket = BucketTypes.Bucket;

    public class BucketStore() {
        private var buckets: HashMap.HashMap<UserId, Bucket> = HashMap.HashMap<UserId, Bucket>(10, Utils.isPrincipalEqual, Principal.hash);

        private let canisterUtils: CanisterUtils.CanisterUtils = CanisterUtils.CanisterUtils();

        public func init(manager: Principal, user: UserId, initNewBucket: (manager: Principal, user: UserId) -> async (Principal)): async (Result.Result<Bucket, Text>) {
            let result: Result.Result<?Bucket, Text> = getBucket(user);

            switch (result) {
                case (#err error) {
                    return #err error;
                };
                case (#ok bucket) {
                    switch (bucket) {
                        case (?bucket) {
                            return #ok bucket;
                        };
                        case null {
                            // TODO: insert partial bucket in store first and handle the partial state in frontend

                            let newBucketId: Principal = await initNewBucket(manager, user);

                            let newDataBucket: BucketTypes.Bucket = {
                                bucketId = ?newBucketId;
                                owner = user;
                            };

                            buckets.put(user, newDataBucket);

                            return #ok newDataBucket;
                        };
                    }
                };
            };
        };

        public func getBucket(user: UserId): Result.Result<?Bucket, Text> {
            let bucket: ?Bucket = buckets.get(user);

            switch bucket {
                case (?{owner}) {
                    if (Utils.isPrincipalEqual(user, owner)) {
                        return #ok bucket;
                    };
                };
                case null {
                    return #ok null;
                };
            };

            return #err "User does not have the permission for the bucket.";
        };

        public func deleteBucket(user: UserId) : async (Result.Result<?Bucket, Text>) {
            let bucket: Result.Result<?Bucket, Text> = getBucket(user);

            switch (bucket) {
                case (#err error) {
                    return #err error;
                };
                case (#ok bucket) {
                    switch (bucket) {
                        case (?{bucketId}) {
                            await canisterUtils.deleteCanister(bucketId);

                            buckets.delete(user);
                        };
                        case null {};
                    };

                    return #ok bucket;
                };
            };
        };

        public func entries(): [Bucket] {
            let entries: Iter.Iter<(UserId, Bucket)> = buckets.entries();
            let values: Iter.Iter<Bucket> = Iter.map(entries, func ((key: UserId, value: Bucket)) : Bucket {
                {
                    bucketId = value.bucketId;
                    owner = value.owner;
                };
             });
            return Iter.toArray(values);
        };

        public func preupgrade(): HashMap.HashMap<UserId, Bucket> {
            return buckets;
        };

        public func postupgrade(stableBuckets: [(UserId, Bucket)]) {
            buckets := HashMap.fromIter<UserId, Bucket>(stableBuckets.vals(), 10, Utils.isPrincipalEqual, Principal.hash);
        };
    }

}
