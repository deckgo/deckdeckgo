import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Cycles "mo:base/ExperimentalCycles";

import Error "mo:base/Error";

import Types "../types/types";

import ManagerTypes "./manager.types";

import DecksStore "./decks.store";
import StoragesStore "./storages.store";

import DeckBucket "../deck/deck";
import StorageBucket "../storage/storage";

actor Manager {
    type DeckId = Types.DeckId;

    type DeckBucket = DeckBucket.DeckBucket;
    type StorageBucket = StorageBucket.StorageBucket;

    type BucketId = ManagerTypes.BucketId;

    type OwnerDeckBucket = ManagerTypes.OwnerBucket<DeckBucket>;
    type OwnerStorageBucket = ManagerTypes.OwnerBucket<StorageBucket>;

    let decksStore: DecksStore.DecksStore = DecksStore.DecksStore();
    let storagesStore: StoragesStore.StoragesStore = StoragesStore.StoragesStore();

    // Preserve the application state on upgrades
    private stable var decks : [(Principal, [(DeckId, OwnerDeckBucket)])] = [];
    private stable var storages : [(Principal, OwnerStorageBucket)] = [];

    /**
     * Decks
     */

    public shared({ caller }) func initDeck(deckId: DeckId): async (BucketId) {
        let self: Principal = Principal.fromActor(Manager);

        let result: {#bucketId: BucketId; #error: Text;} = await decksStore.init(self, caller, deckId);

        switch (result) {
            case (#error error) {
                throw Error.reject(error);
            };
            case (#bucketId bucketId) {
                return bucketId;
            };
        };
    };

    public shared query({ caller }) func getDeck(deckId : DeckId) : async ?BucketId {
        let result: {#bucketId: ?BucketId; #error: Text;} = decksStore.getDeck(caller, deckId);

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
                        // For performance reason, in web app we first query if the deck exists and then if not, we init it
                        // Most ofen the deck will exist already
                        return null;
                    };
                };
            };
        };
    };

    public shared query({ caller }) func deckEntries() : async [BucketId] {
        let result: {#bucketIds: [BucketId]; #error: Text;} = decksStore.getDecks(caller);

        switch (result) {
            case (#error error) {
                throw Error.reject(error);
            };
            case (#bucketIds bucketIds) {
                return bucketIds;
            };
        };
    };

    public shared({ caller }) func delDeck(deckId : DeckId) : async (Bool) {
        let result: {#bucketId: ?BucketId; #error: Text;} = await decksStore.deleteDeck(caller, deckId);

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

    // TODO: inter-canister call secure caller === user canister or this canister

    public func deleteDecksAdmin(user: Principal) : async () {
        let error: ?Text = await decksStore.deleteDecks(user);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {};
        };
    };

    /**
     * Storages
     */

    public shared({ caller }) func initStorage(): async (BucketId) {
        let self: Principal = Principal.fromActor(Manager);

        let result: {#bucketId: BucketId; #error: Text;} = await storagesStore.init(self, caller);

        switch (result) {
            case (#error error) {
                throw Error.reject(error);
            };
            case (#bucketId bucketId) {
                return bucketId;
            };
        };
    };

    public shared query({ caller }) func getStorage() : async ?BucketId {
        let result: {#bucketId: ?BucketId; #error: Text;} = storagesStore.getStorage(caller);

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
                        // For performance reason, in web app we first query if the storage exists and then if not, we init it.
                        return null;
                    };
                };
            };
        };
    };

    public shared({ caller }) func delStorage() : async (Bool) {
        let result: {#bucketId: ?BucketId; #error: Text;} = await storagesStore.deleteStorage(caller);

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

    // TODO: inter-canister call secure caller === user canister or this canister

    public func deleteStorageAdmin(user: Principal) : async () {
        let result: {#bucketId: ?BucketId; #error: Text;} = await storagesStore.deleteStorage(user);

        switch (result) {
            case (#error error) {
                throw Error.reject(error);
            };
            case (#bucketId bucketId) {};
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
