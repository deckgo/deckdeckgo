import Cycles "mo:base/ExperimentalCycles";
import Error "mo:base/Error";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Text "mo:base/Text";

import Types "../types/types";

import CanisterUtils "../utils/canister.utils";

import BucketTypes "./bucket.types";
import BucketStore "./bucket.store";

import DataBucket "../data/data";
import StorageBucket "../storage/storage";

actor Manager {
    type UserId = Types.UserId;

    type DataBucket = DataBucket.DataBucket;
    type StorageBucket = StorageBucket.StorageBucket;

    type BucketId = BucketTypes.BucketId;

    private let canisterUtils: CanisterUtils.CanisterUtils = CanisterUtils.CanisterUtils();

    let dataStore: BucketStore.BucketStore = BucketStore.BucketStore();
    let storagesStore: BucketStore.BucketStore = BucketStore.BucketStore();

    // Preserve the application state on upgrades
    private stable var data : [(Principal, BucketTypes.Bucket)] = [];
    private stable var storages : [(Principal, BucketTypes.Bucket)] = [];

    /**
     * Data
     */

    public shared({ caller }) func initData(): async (BucketId) {
        return await initBucket(caller, dataStore, initNewDataBucket);
    };

    private func initNewDataBucket(manager: Principal, user: UserId, data: HashMap.HashMap<UserId, BucketTypes.Bucket>): async (Principal) {
        Cycles.add(1_000_000_000_000);
        let b: DataBucket = await DataBucket.DataBucket(user);

        let canisterId: Principal = Principal.fromActor(b);

        await canisterUtils.updateSettings(canisterId, manager);

        let newDataBucket: BucketTypes.Bucket = {
            bucketId = canisterId;
            owner = user;
        };

        data.put(user, newDataBucket);

        return canisterId;
    };

    public shared query({ caller }) func getData() : async ?BucketId {
        let result: {#bucketId: ?BucketId; #error: Text;} = dataStore.getBucket(caller);

        switch (result) {
            case (#error error) {
                throw Error.reject(error);
            };
            case (#bucketId bucketId) {
                switch (bucketId) {
                    case (?bucketId) {
                        return ?bucketId;
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

    public shared({ caller }) func initStorage(): async (BucketId) {
        return await initBucket(caller, storagesStore, initNewStorageBucket);
    };

    private func initNewStorageBucket(manager: Principal, user: UserId, storages: HashMap.HashMap<UserId, BucketTypes.Bucket>): async (Principal) {
        Cycles.add(1_000_000_000_000);
        let b: StorageBucket = await StorageBucket.StorageBucket(user);

        let canisterId: Principal = Principal.fromActor(b);

        await canisterUtils.updateSettings(canisterId, manager);

        let newStorageBucket: BucketTypes.Bucket = {
            bucketId = canisterId;
            owner = user;
        };

        storages.put(user, newStorageBucket);

        return canisterId;
    };

    public shared query({ caller }) func getStorage() : async ?BucketId {
        let result: {#bucketId: ?BucketId; #error: Text;} = storagesStore.getBucket(caller);

        switch (result) {
            case (#error error) {
                throw Error.reject(error);
            };
            case (#bucketId bucketId) {
                switch (bucketId) {
                    case (?bucketId) {
                        return ?bucketId;
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

    private func initBucket(caller: Principal, store: BucketStore.BucketStore, initNewBucket: (manager: Principal, user: UserId, buckets: HashMap.HashMap<UserId, BucketTypes.Bucket>) -> async (Principal)): async (BucketId) {
        let self: Principal = Principal.fromActor(Manager);

        let result: {#bucketId: BucketId; #error: Text;} = await store.init(self, caller, initNewBucket);

        switch (result) {
            case (#error error) {
                throw Error.reject(error);
            };
            case (#bucketId bucketId) {
                return bucketId;
            };
        };
    };

    private func delBucket(caller: Principal, store: BucketStore.BucketStore) : async (Bool) {
        let result: {#bucketId: ?BucketId; #error: Text;} = await store.deleteBucket(caller);

        switch (result) {
            case (#error error) {
                throw Error.reject(error);
            };
            case (#bucketId bucketId) {
                let exists: Bool = Option.isSome(bucketId);
                return exists;
            };
        };
    };

    /**
     * Admin
     */

    // TODO: protect caller = david
    public shared query({ caller }) func list(store: Text) : async [{bucketId: BucketId; owner: UserId;}] {
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
