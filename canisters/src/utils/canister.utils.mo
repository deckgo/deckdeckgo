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

        // TODO: does not work out
        // Arg [68, 73, 68, 76, 0, 0] ?
        // https://forum.dfinity.org/t/install-code-actor-class-leads-to-error-empty-input-and-too-few-arguments/8984
        public func installCode(canisterId: Principal, owner: UserId, wasmModule: Blob): async() {
            await ic.install_code({
                arg = Principal.toBlob(owner);
                wasm_module = wasmModule;
                mode = #upgrade;
                canister_id = canisterId;
            });
        };

    }

}
