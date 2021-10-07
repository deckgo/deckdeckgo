import Principal "mo:base/Principal";
import Cycles "mo:base/ExperimentalCycles";

import IC "../types/ic";

module {

    public class WalletUtils() {

        private let ic : IC.Self = actor "aaaaa-aa";

        public func transferCycles(caller: Principal): async () {
            let balance: Nat = Cycles.balance();

            // TODO: find the smallet amount of cycles to retain
            // We have to retain some cycles to be able to transfer some
            let cycles: Nat = balance - 100_000_000_000;

            if (cycles > 0) {
                Cycles.add(cycles);
                await ic.deposit_cycles({ canister_id = caller });
            };
        };

    }

}
