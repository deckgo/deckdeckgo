import Principal "mo:base/Principal";

import Types "./types";
import IC "./ic.types";

module {
    type UserId = Types.UserId;

    public type BucketId = IC.canister_id;

    public type Bucket<T> = {
        bucket: T;
        bucketId: BucketId;
        owner: UserId;
    };
};
