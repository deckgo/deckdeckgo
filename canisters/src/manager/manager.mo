import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Cycles "mo:base/ExperimentalCycles";

import Error "mo:base/Error";

import Types "../common/types";

import BucketTypes "./manager.types";
import ManagerStore "./manager.store";

actor Manager {
    type DeckId = Types.DeckId;

    type OwnerDeckBucket = BucketTypes.OwnerDeckBucket;
    type Bucket = BucketTypes.Bucket;
    type Buckets = BucketTypes.Buckets;
    type BucketId = BucketTypes.BucketId;

    let store: ManagerStore.Store = ManagerStore.Store();

    // Preserve the application state on upgrades
    private stable var decks : [(Principal, [(DeckId, OwnerDeckBucket)])] = [];

    public shared({ caller }) func init(deckId: DeckId): async (BucketId) {
        let self: Principal = Principal.fromActor(Manager);

        let ({error; bucketId}): Bucket = await store.init(self, caller, deckId);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                switch (bucketId) {
                    case (?bucketId) {
                        return bucketId;
                    };
                    case null {
                        throw Error.reject("Cannot init deck bucket.");
                    };
                };
            };
        };
    };

    public shared query({ caller }) func get(deckId : DeckId) : async ?BucketId {
        let ({error; bucketId}): Bucket = store.getDeck(caller, deckId);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
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

    public shared query({ caller }) func entries() : async [BucketId] {
        let ({error; bucketIds}): Buckets = store.getDecks(caller);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                return bucketIds;
            };
        };
    };

    public shared({ caller }) func del(deckId : DeckId) : async (Bool) {
        let deck: Bucket = await store.deleteDeck(caller, deckId);

        switch (deck.error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                let exists: Bool = Option.isSome(deck.bucketId);
                return exists;
            };
        };
    };

    // TODO: inter-canister call secure caller === user canister or this canister

    public func deleteDecksAdmin(user: Principal) : async () {
        let error: ?Text = await store.deleteDecks(user);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {};
        };
    };

    system func preupgrade() {
        decks := Iter.toArray(store.preupgrade().entries());
    };

    system func postupgrade() {
        store.postupgrade(decks);
        decks := [];
    };
}
