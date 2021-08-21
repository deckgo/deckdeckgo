import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Cycles "mo:base/ExperimentalCycles";

import Error "mo:base/Error";

import Types "../common/types";
import DecksBucketTypes "./decks.types";

import DecksStore "./decks.store";

actor Decks {
    type DeckId = Types.DeckId;

    type OwnerDeckBucket = DecksBucketTypes.OwnerDeckBucket;
    type ProtectedDeckBucket = DecksBucketTypes.ProtectedDeckBucket;
    type ProtectedDeckBuckets = DecksBucketTypes.ProtectedDeckBuckets;
    type DeckBucketId = DecksBucketTypes.DeckBucketId;

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
        let ({error; bucketIds}): ProtectedDeckBuckets = store.getDecks(caller);

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
        let self: Principal = Principal.fromActor(Decks);

        let deck: ProtectedDeckBucket = await store.deleteDeck(self, caller, deckId);

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
        let self: Principal = Principal.fromActor(Decks);

        let error: ?Text = await store.deleteDecks(self, user);

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
