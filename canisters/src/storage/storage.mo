import Nat "mo:base/Nat";
import Text "mo:base/Text";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Error "mo:base/Error";

import Types "../types/types";
import StorageTypes "./storage.types";

import Utils "../utils/utils";

import StorageStore "./storage.store";

import WalletUtils "../utils/wallet.utils";

actor class StorageBucket(owner: Types.UserId) = this {

    private let BATCH_EXPIRY_NANOS = 300_000_000_000;

    private type Asset = StorageTypes.Asset;

    private let walletUtils: WalletUtils.WalletUtils = WalletUtils.WalletUtils();

    private stable let user: Types.UserId = owner;

    // Preserve the application state on upgrades
    private stable var entries : [(Text, Asset)] = [];

    let storageStore: StorageStore.StorageStore = StorageStore.StorageStore();

    /**
     * Upload
     */

    public shared({caller}) func create_batch({token: Text;}: {token: Text;}) : async ({batchId : Nat}) {
        if (Utils.isPrincipalNotEqual(caller, user)) {
            throw Error.reject("User does not have the permission to upload content.");
        };

        let nextBatchID: Nat = storageStore.createBatch(token);

        return {batchId = nextBatchID};
    };

    /**
    * Canister mgmt
    */

    // TODO: inter-canister call secure caller === manager canister !!!
    // Or as only controllers can execute following is enough security?

    public shared({ caller }) func transferCycles(): async() {
        await walletUtils.transferCycles(caller);
    };

    system func preupgrade() {
        entries := Iter.toArray(storageStore.preupgrade().entries());
    };

    system func postupgrade() {
        storageStore.postupgrade(entries);
        entries := [];
    };
}