import Principal "mo:base/Principal";

import Types "../types/types";
import IC "../types/ic.types";
import CanisterTypes "../types/canister.types";

module {

    type UserId = Types.UserId;

    type BucketId = CanisterTypes.BucketId;

    public class CanisterUtils() {

        private let ic : IC.Self = actor "aaaaa-aa";

        public func deleteCanister(bucketId: BucketId): async() {
            let deckBucket = actor(Principal.toText(bucketId)): actor { transferCycles: () -> async () };

            await deckBucket.transferCycles();

            await ic.stop_canister({ canister_id = bucketId });

            await ic.delete_canister({ canister_id = bucketId });
        };

        public func updateSettings(canisterId: Principal, manager: Principal): async () {
            let controllers: ?[Principal] = ?[canisterId, manager];

            await ic.update_settings(({canister_id = canisterId; settings = {
                controllers = controllers;
                freezing_threshold = null;
                memory_allocation = null;
                compute_allocation = null;
            }}));
        };

        public func installCode(canisterId: Principal, wasmModule: Blob): async() {
            await ic.install_code({
                arg = "";
                wasm_module = wasmModule;
                mode = #upgrade;
                canister_id = canisterId;
            });
        };

    }

}
