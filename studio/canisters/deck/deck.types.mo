import Principal "mo:base/Principal";

import Types "../common/types";

import DeckBucket "./deck";

module {
    type UserId = Types.UserId;

    type DeckBucket = DeckBucket.DeckBucket;

    public type DeckBucketId = Principal;

    public type OwnerDeckBucket = {
        bucket: DeckBucket;
        bucketId: DeckBucketId;
        owner: UserId;
    };

    public type ProtectedDeckBucket = {
        bucketId: ?DeckBucketId;
        error: ?Text;
    };

    public type ProtectedDeckBuckets = {
        buckets: [DeckBucketId];
        error: ?Text;
    };
};
