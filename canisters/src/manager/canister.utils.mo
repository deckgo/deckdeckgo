import Principal "mo:base/Principal";

import Types "../common/types";
import BucketTypes "./manager.types";

import IC "../common/ic";

module {

    type UserId = Types.UserId;

    type BucketId = BucketTypes.BucketId;

    public class Utils() {

        private let ic : IC.Self = actor "aaaaa-aa";

        public func deleteCanister(bucketId: BucketId): async() {
            let deckBucket = actor(Principal.toText(bucketId)): actor { transferCycles: () -> async () };

            await deckBucket.transferCycles();

            await ic.stop_canister({ canister_id = bucketId });

            await ic.delete_canister({ canister_id = bucketId });
        };

        public func updateSettings(canisterId: Principal, manager: Principal, user: UserId): async () {
            let controllers: ?[Principal] = ?[canisterId, user, manager];

            await ic.update_settings(({canister_id = canisterId; settings = {
                controllers = controllers;
                freezing_threshold = null;
                memory_allocation = null;
                compute_allocation = null;
            }}));
        }

    }

}