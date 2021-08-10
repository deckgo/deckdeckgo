import Types "../common/types";

import DeckBucket "./deck.bucket";

module {
    type UserId = Types.UserId;

    type DeckBucket = DeckBucket.DeckBucket;

    public type OwnerDeckBucket = {
        bucket: DeckBucket;
        owner: UserId;
    };

    public type ProtectedDeckBucket = {
        bucket: ?DeckBucket;
        error: ?Text;
    };

    public type ProtectedDeckBuckets = {
        buckets: [DeckBucket];
        error: ?Text;
    };
};
