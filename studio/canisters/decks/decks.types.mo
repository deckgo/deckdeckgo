import Principal "mo:base/Principal";

import Types "../common/types";
import IC "../common/ic";

import DeckBucket "../deck/deck";

module {
    type UserId = Types.UserId;

    type DeckBucket = DeckBucket.DeckBucket;

    public type DeckBucketId = IC.canister_id;

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
        bucketIds: [DeckBucketId];
        error: ?Text;
    };
};
