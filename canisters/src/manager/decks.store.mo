import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Cycles "mo:base/ExperimentalCycles";

import Error "mo:base/Error";

import Types "../types/types";
import CanisterTypes "../types/canister.types";

import Utils "../utils/utils";

import DeckBucket "../deck/deck";

import CanisterUtils "../utils/canister.utils";

module {
    type UserId = Types.UserId;
    type DeckId = Types.DeckId;
    type SlideId = Types.SlideId;

    type DeckBucket = DeckBucket.DeckBucket;

    type BucketId = CanisterTypes.BucketId;
    type OwnerDeckBucket = CanisterTypes.Bucket<DeckBucket>;

    public class DecksStore() {
        private var decks: HashMap.HashMap<UserId, HashMap.HashMap<DeckId, OwnerDeckBucket>> = HashMap.HashMap<UserId, HashMap.HashMap<DeckId, OwnerDeckBucket>>(10, Utils.isPrincipalEqual, Principal.hash);

        private let canisterUtils: CanisterUtils.CanisterUtils = CanisterUtils.CanisterUtils();

        public func init(manager: Principal, user: UserId, deckId: DeckId): async ({#bucketId: BucketId; #error: Text;}) {
            let deckBucket: {#bucketId: ?BucketId; #error: Text;} = getDeck(user, deckId);

            switch (deckBucket) {
                case (#error error) {
                    return #error error;
                };
                case (#bucketId bucketId) {
                    switch (bucketId) {
                        case (?bucketId) {
                            return #bucketId bucketId;
                        };
                        case null {
                            let b: DeckBucket = await initNewBucket(manager, user);

                            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeckBucket> = decks.get(user);

                            let newBucketId: Principal = Principal.fromActor(b);

                            let newDeckBucket: OwnerDeckBucket = {
                                bucket = b;
                                bucketId = newBucketId;
                                owner = user;
                            };

                            switch ownerDecks {
                                case (?ownerDecks) {
                                    setOwnerDeck(user, deckId, newDeckBucket, ownerDecks);
                                };
                                case null {
                                    let ownerDecks: HashMap.HashMap<DeckId, OwnerDeckBucket> = HashMap.HashMap<DeckId, OwnerDeckBucket>(10, Text.equal, Text.hash);

                                    setOwnerDeck(user, deckId, newDeckBucket, ownerDecks);
                                }
                            };

                            return #bucketId newBucketId;
                        };
                    }
                };
            };
        };

        private func initNewBucket(manager: Principal, user: UserId): async (DeckBucket) {
            Cycles.add(1_000_000_000_000);
            let b: DeckBucket = await DeckBucket.DeckBucket(user);

            let canisterId: Principal = Principal.fromActor(b);

            await canisterUtils.updateSettings(canisterId, manager, user);

            return b;
        };

        private func setOwnerDeck(user: UserId, deckId: DeckId, newDeckBucket: OwnerDeckBucket, ownerDecks: HashMap.HashMap<DeckId, OwnerDeckBucket>) {
            ownerDecks.put(deckId, newDeckBucket);
            decks.put(user, ownerDecks);
        };

        public func getDeck(user: UserId, deckId: DeckId): {#bucketId: ?BucketId; #error: Text;} {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeckBucket> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    return getOwnerDeck(user, deckId, ownerDecks);
                };
                case null {
                    return #bucketId null;
                };
            };
        };

        public func getDecks(user: UserId): {#bucketIds: [BucketId]; #error: Text;} {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeckBucket> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    var results: ([BucketId]) = [];

                    for ((deckId: DeckId, value: OwnerDeckBucket) in ownerDecks.entries()) {
                        if (Utils.isPrincipalNotEqual(user, value.owner)) {
                            return #error "User does not have the permission for one of the deck.";
                        };

                        results := Array.append(results, [value.bucketId]);
                    };

                    return #bucketIds results;
                };
                case null {
                    let empty: [BucketId] = [];
                    return #bucketIds empty;
                }
            };
        };

        private func getOwnerDeck(user: UserId, deckId: DeckId, ownerDecks: HashMap.HashMap<DeckId, OwnerDeckBucket>): {#bucketId: ?BucketId; #error: Text;} {
            let ownerDeck: ?OwnerDeckBucket = ownerDecks.get(deckId);

            switch ownerDeck {
                case (?ownerDeck) {
                    if (Utils.isPrincipalEqual(user, ownerDeck.owner)) {
                        let bucketId: ?BucketId = ?ownerDeck.bucketId;
                        return #bucketId bucketId;
                    };
                };
                case null {
                    return #bucketId null;
                };
            };

            return #error "User does not have the permission for the deck.";
        };

        public func deleteDeck(user: UserId, deckId: DeckId) : async ({#bucketId: ?BucketId; #error: Text;}) {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeckBucket> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    let protectedDeck: {#bucketId: ?BucketId; #error: Text;} = getOwnerDeck(user, deckId, ownerDecks);

                    switch (protectedDeck) {
                        case (#error error) {
                            return #error error;
                        };
                        case (#bucketId bucketId) {
                            switch (bucketId) {
                                case (?bucketId) {
                                    await canisterUtils.deleteCanister(bucketId);

                                    let removedDeck: ?OwnerDeckBucket = ownerDecks.remove(deckId);
                                    decks.put(user, ownerDecks);
                                };
                                case null {};
                            };

                            return #bucketId bucketId;
                        };
                    };
                };
                case null {
                    return #bucketId null;
                };
            };
        };

        public func deleteDecks(user: UserId) : async (?Text) {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeckBucket> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    for ((deckId: DeckId, value: OwnerDeckBucket) in ownerDecks.entries()) {
                        if (Utils.isPrincipalNotEqual(user, value.owner)) {
                            return ?"User does not have the permission for one of the deck.";
                        };
                    };

                    for ((deckId: DeckId, value: OwnerDeckBucket) in ownerDecks.entries()) {
                        await canisterUtils.deleteCanister(value.bucketId);
                    };

                    let removedDecks: ?HashMap.HashMap<DeckId, OwnerDeckBucket> = decks.remove(user);

                    return null;
                };
                case null {
                    return null;
                }
            };
        };

        public func preupgrade(): HashMap.HashMap<UserId, [(DeckId, OwnerDeckBucket)]> {
            let entries : HashMap.HashMap<UserId, [(DeckId, OwnerDeckBucket)]> = HashMap.HashMap<UserId, [(DeckId, OwnerDeckBucket)]>(10, Utils.isPrincipalEqual, Principal.hash);

            for ((key: UserId, value: HashMap.HashMap<DeckId, OwnerDeckBucket>) in decks.entries()) {
                let ownerDecks : [(DeckId, OwnerDeckBucket)] = Iter.toArray<(DeckId, OwnerDeckBucket)>(value.entries());
                entries.put(key, ownerDecks);
            };

            return entries;
        };

        public func postupgrade(entries: [(UserId, [(DeckId, OwnerDeckBucket)])]) {
            for ((key: UserId, value: [(DeckId, OwnerDeckBucket)]) in entries.vals()) {
                let ownerDecks: HashMap.HashMap<DeckId, OwnerDeckBucket> = HashMap.fromIter<DeckId, OwnerDeckBucket>(Iter.fromArray<(DeckId, OwnerDeckBucket)>(value), 10, Text.equal, Text.hash);

                decks.put(key, ownerDecks);
            };
        };
    }

}
