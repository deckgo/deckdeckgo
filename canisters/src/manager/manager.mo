import Cycles "mo:base/ExperimentalCycles";
import Error "mo:base/Error";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Result "mo:base/Result";

import Types "../types/types";

import CanisterUtils "../utils/canister.utils";

import BucketTypes "./bucket.types";
import BucketStore "./bucket.store";

import DataBucket "../data/data";
import StorageBucket "../storage/storage";

actor Manager {
    private type UserId = Types.UserId;

    private type DataBucket = DataBucket.DataBucket;
    private type StorageBucket = StorageBucket.StorageBucket;

    private type Bucket = BucketTypes.Bucket;

    private let canisterUtils: CanisterUtils.CanisterUtils = CanisterUtils.CanisterUtils();

    let dataStore: BucketStore.BucketStore = BucketStore.BucketStore();
    let storagesStore: BucketStore.BucketStore = BucketStore.BucketStore();

    // Preserve the application state on upgrades
    private stable var data : [(Principal, BucketTypes.Bucket)] = [];
    private stable var storages : [(Principal, BucketTypes.Bucket)] = [];

    /**
     * Data
     */

    public shared({ caller }) func initData(): async (Bucket) {
        return await initBucket(caller, dataStore, initNewDataBucket);
    };

    private func initNewDataBucket(manager: Principal, user: UserId): async (Principal) {
        Cycles.add(1_000_000_000_000);
        let b: DataBucket = await DataBucket.DataBucket(user);

        let canisterId: Principal = Principal.fromActor(b);

        await canisterUtils.updateSettings(canisterId, manager);

        return canisterId;
    };

    public shared query({ caller }) func getData() : async ?Bucket {
        let result: Result.Result<?Bucket, Text> = dataStore.getBucket(caller);

        switch (result) {
            case (#err error) {
                throw Error.reject(error);
            };
            case (#ok bucket) {
                switch (bucket) {
                    case (?bucket) {
                        return ?bucket;
                    };
                    case null {
                        // We do not throw a "Not found error" here.
                        // For performance reason, in web app we first query if the bucket exists and then if not, we init it.
                        return null;
                    };
                };
            };
        };
    };

    public shared({ caller }) func delData() : async (Bool) {
        return await delBucket(caller, dataStore);
    };

    /**
     * Storages
     */

    public shared({ caller }) func initStorage(): async (Bucket) {
        return await initBucket(caller, storagesStore, initNewStorageBucket);
    };

    private func initNewStorageBucket(manager: Principal, user: UserId): async (Principal) {
        Cycles.add(1_000_000_000_000);
        let b: StorageBucket = await StorageBucket.StorageBucket(user);

        let canisterId: Principal = Principal.fromActor(b);

        await canisterUtils.updateSettings(canisterId, manager);

        return canisterId;
    };

    public shared query({ caller }) func getStorage() : async ?Bucket {
        let result: Result.Result<?Bucket, Text> = storagesStore.getBucket(caller);

        switch (result) {
            case (#err error) {
                throw Error.reject(error);
            };
            case (#ok bucket) {
                switch (bucket) {
                    case (?bucket) {
                        return ?bucket;
                    };
                    case null {
                        // We do not throw a "Not found error" here.
                        // For performance reason, in web app we first query if the bucket exists and then if not, we init it.
                        return null;
                    };
                };
            };
        };
    };

    public shared({ caller }) func delStorage() : async (Bool) {
        return await delBucket(caller, storagesStore);
    };

    /**
     * Buckets
     */

    private func initBucket(caller: Principal, store: BucketStore.BucketStore, initNewBucket: (manager: Principal, user: UserId) -> async (Principal)): async (Bucket) {
        let self: Principal = Principal.fromActor(Manager);

        let result: Result.Result<Bucket, Text> = await store.init(self, caller, initNewBucket);

        switch (result) {
            case (#err error) {
                throw Error.reject(error);
            };
            case (#ok bucket) {
                return bucket;
            };
        };
    };

    private func delBucket(caller: Principal, store: BucketStore.BucketStore) : async (Bool) {
        let result: Result.Result<?Bucket, Text> = await store.deleteBucket(caller);

        switch (result) {
            case (#err error) {
                throw Error.reject(error);
            };
            case (#ok bucket) {
                let exists: Bool = Option.isSome(bucket);
                return exists;
            };
        };
    };

    /**
     * Admin
     */

    // TODO: protect caller = david
    public shared query({ caller }) func list(store: Text) : async [Bucket] {
        if (Text.equal(store, "data")) {
            return dataStore.entries();
        };

        if (Text.equal(store, "storage")) {
            return storagesStore.entries();
        };

        throw Error.reject("Type of store not supported");
    };

    // TODO: protect caller = david
    public shared({ caller }) func installCode(canisterId: Principal, owner: UserId, wasmModule: Blob): async() {
        await canisterUtils.installCode(canisterId, owner, wasmModule);
    };

    /**
     * Stable memory for upgrade
     */

    system func preupgrade() {
        data := Iter.toArray(dataStore.preupgrade().entries());
        storages := Iter.toArray(storagesStore.preupgrade().entries());
    };

    system func postupgrade() {
        dataStore.postupgrade(data);
        data := [];

        storagesStore.postupgrade(storages);
        storages := [];
    };
}
