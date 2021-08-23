import Principal "mo:base/Principal";

import Types "../common/types";
import IC "../common/ic";

import DeckBucket "../deck/deck";

module {
    type UserId = Types.UserId;

    type DeckBucket = DeckBucket.DeckBucket;

    public type BucketId = IC.canister_id;

    public type OwnerBucket = {
        bucket: DeckBucket;
        bucketId: BucketId;
        owner: UserId;
    };

    public type ProtectedBucket = {
        bucketId: ?BucketId;
        error: ?Text;
    };

    public type ProtectedBuckets = {
        bucketIds: [BucketId];
        error: ?Text;
    };
};
