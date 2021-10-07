import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Cycles "mo:base/ExperimentalCycles";

import Error "mo:base/Error";

import Types "../common/types";

import BucketTypes "./manager.types";
import DecksStore "./decks.store";

actor Manager {
    type DeckId = Types.DeckId;

    type OwnerDeckBucket = BucketTypes.OwnerDeckBucket;

    type Bucket = BucketTypes.Bucket;
    type Buckets = BucketTypes.Buckets;
    type BucketId = BucketTypes.BucketId;

    let decksStore: DecksStore.DecksStore = DecksStore.DecksStore();

    // Preserve the application state on upgrades
    private stable var decks : [(Principal, [(DeckId, OwnerDeckBucket)])] = [];

    public shared({ caller }) func init(deckId: DeckId): async (BucketId) {
        let self: Principal = Principal.fromActor(Manager);

        let ({error; bucketId}): Bucket = await decksStore.init(self, caller, deckId);

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
        let ({error; bucketId}): Bucket = decksStore.getDeck(caller, deckId);

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
        let ({error; bucketIds}): Buckets = decksStore.getDecks(caller);

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
        let deck: Bucket = await decksStore.deleteDeck(caller, deckId);

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
        let error: ?Text = await decksStore.deleteDecks(user);

        switch (error) {
            case (?error) {
                throw Error.reject(error);
            };
            case null {};
        };
    };

    system func preupgrade() {
        decks := Iter.toArray(decksStore.preupgrade().entries());
    };

    system func postupgrade() {
        decksStore.postupgrade(decks);
        decks := [];
    };
}
