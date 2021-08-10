import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Cycles "mo:base/ExperimentalCycles";

import Error "mo:base/Error";

import Types "../common/types";
import DeckBucketTypes "../deck/deck.types";

import DecksStore "./decks.store";

actor Decks {
    type DeckId = Types.DeckId;

    type OwnerDeckBucket = DeckBucketTypes.OwnerDeckBucket;
    type ProtectedDeckBucket = DeckBucketTypes.ProtectedDeckBucket;
    type ProtectedDeckBuckets = DeckBucketTypes.ProtectedDeckBuckets;
    type DeckBucketId = DeckBucketTypes.DeckBucketId;

    let store: DecksStore.Store = DecksStore.Store();

    // Preserve the application state on upgrades
    private stable var decks : [(Principal, [(DeckId, OwnerDeckBucket)])] = [];

    public shared({ caller }) func init(deckId: DeckId): async (DeckBucketId) {
        let self: Principal = Principal.fromActor(Decks);

        let ({error; bucketId}): ProtectedDeckBucket = await store.init(self, caller, deckId);

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

    public shared query({ caller }) func get(deckId : DeckId) : async DeckBucketId {
        let ({error; bucketId}): ProtectedDeckBucket = store.getDeck(caller, deckId);

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
                        throw Error.reject("Deck bucket not found.");
                    };
                };
            };
        };
    };

    public shared query({ caller }) func entries() : async [DeckBucketId] {
        let ({error; buckets}): ProtectedDeckBuckets = store.getDecks(caller);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                return buckets;
            };
        };
    };

    // TODO: inter-canister call secure caller === user canister

    public func entriesAdmin(user: Principal) : async [DeckBucketId] {
        let ({error; buckets}): ProtectedDeckBuckets = store.getDecks(user);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {
                return buckets;
            };
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
