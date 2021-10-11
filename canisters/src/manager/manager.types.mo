import Principal "mo:base/Principal";

import Types "../types/types";
import IC "../types/ic";

module {
    type UserId = Types.UserId;

    public type BucketId = IC.canister_id;

    public type OwnerBucket<T> = {
        bucket: T;
        bucketId: BucketId;
        owner: UserId;
    };
};
