import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Cycles "mo:base/ExperimentalCycles";

import Error "mo:base/Error";

import Types "../common/types";
import BucketTypes "./manager.types";

import Utils "../common/utils";
import IC "../common/ic";

import DeckBucket "../deck/deck";

module {
    type UserId = Types.UserId;
    type DeckId = Types.DeckId;
    type SlideId = Types.SlideId;

    type OwnerBucket = BucketTypes.OwnerBucket;
    type ProtectedBucket = BucketTypes.ProtectedBucket;
    type ProtectedBuckets = BucketTypes.ProtectedBuckets;
    type BucketId = BucketTypes.BucketId;

    type DeckBucket = DeckBucket.DeckBucket;

    public class Store() {
        private var decks: HashMap.HashMap<UserId, HashMap.HashMap<DeckId, OwnerBucket>> = HashMap.HashMap<UserId, HashMap.HashMap<DeckId, OwnerBucket>>(10, Utils.isPrincipalEqual, Principal.hash);

        private let ic : IC.Self = actor "aaaaa-aa";

        public func init(manager: Principal, user: UserId, deckId: DeckId): async (ProtectedBucket) {
            let deckBucket: ProtectedBucket = getDeck(user, deckId);

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

                            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerBucket> = decks.get(user);

                            let newDeckBucket: OwnerBucket = {
                                bucket = b;
                                bucketId = await b.id();
                                owner = user;
                            };

                            switch ownerDecks {
                                case (?ownerDecks) {
                                    setOwnerDeck(user, deckId, newDeckBucket, ownerDecks);
                                };
                                case null {
                                    let ownerDecks: HashMap.HashMap<DeckId, OwnerBucket> = HashMap.HashMap<DeckId, OwnerBucket>(10, Text.equal, Text.hash);

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

            let controllers: ?[Principal] = ?[canisterId, user, manager];

            await ic.update_settings(({canister_id = canisterId; settings = {
                controllers = controllers;
                freezing_threshold = null;
                memory_allocation = null;
                compute_allocation = null;
            }}));

            return b;
        };

        private func setOwnerDeck(user: UserId, deckId: DeckId, newDeckBucket: OwnerBucket, ownerDecks: HashMap.HashMap<DeckId, OwnerBucket>) {
            ownerDecks.put(deckId, newDeckBucket);
            decks.put(user, ownerDecks);
        };

        public func getDeck(user: UserId, deckId: DeckId): ProtectedBucket {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerBucket> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    let ownerDeck: ProtectedBucket = getOwnerDeck(user, deckId, ownerDecks);
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

        public func getDecks(user: UserId): ProtectedBuckets {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerBucket> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    var results: ([BucketId]) = [];

                    for ((deckId: DeckId, value: OwnerBucket) in ownerDecks.entries()) {
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

        private func getOwnerDeck(user: UserId, deckId: DeckId, ownerDecks: HashMap.HashMap<DeckId, OwnerBucket>): ProtectedBucket {
            let ownerDeck: ?OwnerBucket = ownerDecks.get(deckId);

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

        public func deleteDeck(user: UserId, deckId: DeckId) : async (ProtectedBucket) {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerBucket> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    let protectedDeck: ProtectedBucket = getOwnerDeck(user, deckId, ownerDecks);

                    switch (protectedDeck.bucketId) {
                        case (?bucketId) {
                            await deleteCanister(bucketId);

                            let removedDeck: ?OwnerBucket = ownerDecks.remove(deckId);
                            decks.put(user, ownerDecks);
                        };
                        case null {};
                    };

                    return protectedDeck;
                };
                case null {
                    return {
                        bucketId = null;
                        error = null;
                    };
                }
            };
        };

        public func deleteDecks(user: UserId) : async (?Text) {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerBucket> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    for ((deckId: DeckId, value: OwnerBucket) in ownerDecks.entries()) {
                        if (Utils.isPrincipalNotEqual(user, value.owner)) {
                            return ?"User does not have the permission for one of the deck.";
                        };
                    };

                    for ((deckId: DeckId, value: OwnerBucket) in ownerDecks.entries()) {
                        await deleteCanister(value.bucketId);
                    };

                    let removedDecks: ?HashMap.HashMap<DeckId, OwnerBucket> = decks.remove(user);

                    return null;
                };
                case null {
                    return null;
                }
            };
        };

        private func deleteCanister(bucketId: BucketId): async() {
            let deckBucket = actor(Principal.toText(bucketId)): actor { transferCycles: () -> async () };

            await deckBucket.transferCycles();

            await ic.stop_canister({ canister_id = bucketId });

            await ic.delete_canister({ canister_id = bucketId });
        };

        public func preupgrade(): HashMap.HashMap<UserId, [(DeckId, OwnerBucket)]> {
            let entries : HashMap.HashMap<UserId, [(DeckId, OwnerBucket)]> = HashMap.HashMap<UserId, [(DeckId, OwnerBucket)]>(10, Utils.isPrincipalEqual, Principal.hash);

            for ((key: UserId, value: HashMap.HashMap<DeckId, OwnerBucket>) in decks.entries()) {
                let ownerDecks : [(DeckId, OwnerBucket)] = Iter.toArray<(DeckId, OwnerBucket)>(value.entries());
                entries.put(key, ownerDecks);
            };

            return entries;
        };

        public func postupgrade(entries: [(UserId, [(DeckId, OwnerBucket)])]) {
            for ((key: UserId, value: [(DeckId, OwnerBucket)]) in entries.vals()) {
                let ownerDecks: HashMap.HashMap<DeckId, OwnerBucket> = HashMap.fromIter<DeckId, OwnerBucket>(Iter.fromArray<(DeckId, OwnerBucket)>(value), 10, Text.equal, Text.hash);

                decks.put(key, ownerDecks);
            };
        };
    }

}
