import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Cycles "mo:base/ExperimentalCycles";

import Error "mo:base/Error";

import Types "../common/types";
import DeckBucketTypes "../deck/deck.types";

import Utils "../common/utils";
import IC "../common/ic";

import DeckBucket "../deck/deck";

module {
    type UserId = Types.UserId;
    type DeckId = Types.DeckId;
    type SlideId = Types.SlideId;

    type OwnerDeckBucket = DeckBucketTypes.OwnerDeckBucket;
    type ProtectedDeckBucket = DeckBucketTypes.ProtectedDeckBucket;
    type ProtectedDeckBuckets = DeckBucketTypes.ProtectedDeckBuckets;
    type DeckBucketId = DeckBucketTypes.DeckBucketId;

    type DeckBucket = DeckBucket.DeckBucket;

    public class Store() {
        private var decks: HashMap.HashMap<UserId, HashMap.HashMap<DeckId, OwnerDeckBucket>> = HashMap.HashMap<UserId, HashMap.HashMap<DeckId, OwnerDeckBucket>>(10, Utils.isPrincipalEqual, Principal.hash);

        let ic : IC.Self = actor "aaaaa-aa";

        public func init(manager: Principal, user: UserId, deckId: DeckId): async (ProtectedDeckBucket) {
            let deckBucket: ProtectedDeckBucket = getDeck(user, deckId);

            switch (deckBucket.error) {
                case (?error) {
                    return deckBucket;
                };
                case null {
                    switch (deckBucket.bucketId) {
                        case (?bucketId) {
                            return deckBucket;
                        };
                        case null {
                            let b: DeckBucket = await initNewBucket(manager, user);

                            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeckBucket> = decks.get(user);

                            let newDeckBucket: OwnerDeckBucket = {
                                bucket = b;
                                bucketId = await b.id();
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

                            return {
                                bucketId = ?(await b.id());
                                error = null;
                            };
                        };
                    }
                };
            };
        };

        private func initNewBucket(manager: Principal, user: UserId): async (DeckBucket) {
            Cycles.add(1_000_000_000_000);
            let b: DeckBucket = await DeckBucket.DeckBucket(user);

            let canisterId: Principal = await b.id();

            let controllers: ?[Principal] = ?[user, manager];

            await ic.update_settings(({canister_id = canisterId; settings = {
                controllers = controllers;
                freezing_threshold = null;
                memory_allocation = null;
                compute_allocation = null;
            }}));

            return b;
        };

        private func setOwnerDeck(user: UserId, deckId: DeckId, newDeckBucket: OwnerDeckBucket, ownerDecks: HashMap.HashMap<DeckId, OwnerDeckBucket>) {
            ownerDecks.put(deckId, newDeckBucket);
            decks.put(user, ownerDecks);
        };

        public func getDeck(user: UserId, deckId: DeckId): ProtectedDeckBucket {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeckBucket> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    let ownerDeck: ProtectedDeckBucket = getOwnerDeck(user, deckId, ownerDecks);
                    return ownerDeck;
                };
                case null {
                    return {
                        bucketId = null;
                        error = null;
                    };
                };
            };
        };

        public func getDecks(user: UserId): ProtectedDeckBuckets {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeckBucket> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    var results: ([DeckBucketId]) = [];

                    for ((deckId: DeckId, value: OwnerDeckBucket) in ownerDecks.entries()) {
                        if (Utils.isPrincipalNotEqual(user, value.owner)) {
                            return {
                                bucketIds = [];
                                error = ?"User does not have the permission for one of the deck.";
                            };
                        };

                        results := Array.append(results, [value.bucketId]);
                    };

                    return {
                        bucketIds = results;
                        error = null;
                    };
                };
                case null {
                    return {
                        bucketIds = [];
                        error = null;
                    }
                }
            };
        };

        private func getOwnerDeck(user: UserId, deckId: DeckId, ownerDecks: HashMap.HashMap<DeckId, OwnerDeckBucket>): ProtectedDeckBucket {
            let ownerDeck: ?OwnerDeckBucket = ownerDecks.get(deckId);

            switch ownerDeck {
                case (?ownerDeck) {
                    if (Utils.isPrincipalEqual(user, ownerDeck.owner)) {
                        return {
                            bucketId = ?ownerDeck.bucketId;
                            error = null;
                        };
                    };
                };
                case null {
                    return {
                        bucketId = null;
                        error = null;
                    };
                };
            };

            return {
                bucketId = null;
                error = ?"User does not have the permission for the deck.";
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
