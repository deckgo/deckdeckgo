import Principal "mo:base/Principal";

import Types "../types/types";
import IC "../types/ic";

import DeckBucket "../deck/deck";

module {
    type UserId = Types.UserId;

    type DeckBucket = DeckBucket.DeckBucket;

    public type BucketId = IC.canister_id;

    public type OwnerDeckBucket = {
        bucket: DeckBucket;
        bucketId: BucketId;
        owner: UserId;
    };

    public type Bucket = {
        bucketId: ?BucketId;
        error: ?Text;
    };

    public type Buckets = {
        bucketIds: [BucketId];
        error: ?Text;
    };
};
