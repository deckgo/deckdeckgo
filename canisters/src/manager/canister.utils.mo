import Principal "mo:base/Principal";

import BucketTypes "./manager.types";

import IC "../common/ic";

module {

    type BucketId = BucketTypes.BucketId;

    public class Utils() {

        private let ic : IC.Self = actor "aaaaa-aa";

        public func deleteCanister(bucketId: BucketId): async() {
            let deckBucket = actor(Principal.toText(bucketId)): actor { transferCycles: () -> async () };

            await deckBucket.transferCycles();

            await ic.stop_canister({ canister_id = bucketId });

            await ic.delete_canister({ canister_id = bucketId });
        };

    }

}