import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Cycles "mo:base/ExperimentalCycles";

import Error "mo:base/Error";

import Types "../types/types";
import CanisterTypes "../types/canister.types";

import BucketsStore "./buckets.store";

import DeckBucket "../deck/deck";
import StorageBucket "../storage/storage";

import CanisterUtils "../utils/canister.utils";

actor Manager {
    type DeckId = Types.DeckId;
    type UserId = Types.UserId;

    type DeckBucket = DeckBucket.DeckBucket;
    type StorageBucket = StorageBucket.StorageBucket;

    type BucketId = CanisterTypes.BucketId;

    type OwnerDeckBucket = CanisterTypes.Bucket<DeckBucket>;

    private let canisterUtils: CanisterUtils.CanisterUtils = CanisterUtils.CanisterUtils();

    let decksStore: BucketsStore.BucketsStore<DeckBucket> = BucketsStore.BucketsStore<DeckBucket>();
    let storagesStore: BucketsStore.BucketsStore<StorageBucket> = BucketsStore.BucketsStore<StorageBucket>();

    // Preserve the application state on upgrades
    private stable var decks : [(Principal, CanisterTypes.Bucket<DeckBucket>)] = [];
    private stable var storages : [(Principal, CanisterTypes.Bucket<StorageBucket>)] = [];

    /**
     * Decks
     */

    public shared({ caller }) func initDeck(): async (BucketId) {
        return await initBucket<DeckBucket>(caller, decksStore, initNewDeckBucket);
    };

    private func initNewDeckBucket(manager: Principal, user: UserId, decks: HashMap.HashMap<UserId, CanisterTypes.Bucket<DeckBucket>>): async (Principal) {
        Cycles.add(1_000_000_000_000);
        let b: DeckBucket = await DeckBucket.DeckBucket(user);

        let canisterId: Principal = Principal.fromActor(b);

        await canisterUtils.updateSettings(canisterId, manager, user);

        let newDeckBucket: CanisterTypes.Bucket<DeckBucket> = {
            bucket = b;
            bucketId = canisterId;
            owner = user;
        };

        decks.put(user, newDeckBucket);

        return canisterId;
    };

    public shared query({ caller }) func getDeck() : async ?BucketId {
        let result: {#bucketId: ?BucketId; #error: Text;} = decksStore.getBucket(caller);

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

    public shared({ caller }) func delDeck() : async (Bool) {
        return await delBucket<DeckBucket>(caller, decksStore);
    };

    // TODO: do we need inter-canister call or do we solves this in another way?
    // TODO: inter-canister call secure caller === user canister or this canister

    public func deleteDeckAdmin(user: Principal) : async (Bool) {
        return await delBucket<DeckBucket>(user, decksStore);
    };

    /**
     * Storages
     */

    public shared({ caller }) func initStorage(): async (BucketId) {
        return await initBucket<StorageBucket>(caller, storagesStore, initNewStorageBucket);
    };

    private func initNewStorageBucket(manager: Principal, user: UserId, storages: HashMap.HashMap<UserId, CanisterTypes.Bucket<StorageBucket>>): async (Principal) {
        Cycles.add(1_000_000_000_000);
        let b: StorageBucket = await StorageBucket.StorageBucket(user);

        let canisterId: Principal = Principal.fromActor(b);

        await canisterUtils.updateSettings(canisterId, manager, user);

        let newStorageBucket: CanisterTypes.Bucket<StorageBucket> = {
            bucket = b;
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
        return await delBucket<StorageBucket>(caller, storagesStore);
    };

    // TODO: do we need inter-canister call or do we solves this in another way?
    // TODO: inter-canister call secure caller === user canister or this canister

    public func deleteStorageAdmin(user: Principal) : async (Bool) {
        return await delBucket<StorageBucket>(user, storagesStore);
    };

    /**
     * Buckets
     */

    private func initBucket<T>(caller: Principal, store: BucketsStore.BucketsStore<T>, initNewBucket: (manager: Principal, user: UserId, buckets: HashMap.HashMap<UserId, CanisterTypes.Bucket<T>>) -> async (Principal)): async (BucketId) {
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

    private func delBucket<T>(caller: Principal, store: BucketsStore.BucketsStore<T>) : async (Bool) {
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
     * Stable memory for upgrade
     */

    system func preupgrade() {
        decks := Iter.toArray(decksStore.preupgrade().entries());
        storages := Iter.toArray(storagesStore.preupgrade().entries());
    };

    system func postupgrade() {
        decksStore.postupgrade(decks);
        decks := [];

        storagesStore.postupgrade(storages);
        storages := [];
    };
}
