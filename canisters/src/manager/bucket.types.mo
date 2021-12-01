import Principal "mo:base/Principal";

import Types "../types/types";
import IC "../types/ic.types";

module {
    public type BucketId = IC.canister_id;

    public type Bucket = {
        bucketId: BucketId;
        owner: Types.UserId;
    };
};
