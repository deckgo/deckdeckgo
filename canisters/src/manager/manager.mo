import Iter "mo:base/Iter";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Cycles "mo:base/ExperimentalCycles";

import Error "mo:base/Error";

import Types "../types/types";

import ManagerTypes "./manager.types";
import DecksStore "./decks.store";

actor Manager {
    type DeckId = Types.DeckId;

    type BucketId = ManagerTypes.BucketId;
    type OwnerDeckBucket = ManagerTypes.OwnerDeckBucket;

    let decksStore: DecksStore.DecksStore = DecksStore.DecksStore();

    // Preserve the application state on upgrades
    private stable var decks : [(Principal, [(DeckId, OwnerDeckBucket)])] = [];

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

    system func preupgrade() {
        decks := Iter.toArray(decksStore.preupgrade().entries());
    };

    system func postupgrade() {
        decksStore.postupgrade(decks);
        decks := [];
    };
}
