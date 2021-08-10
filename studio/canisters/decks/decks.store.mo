import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Cycles "mo:base/ExperimentalCycles";

import Error "mo:base/Error";

import Types "../common/types";
import DeckBucketTypes "./deck.bucket.types";

import Utils "../common/utils";

import DeckBucket "./deck.bucket";

module {
    type UserId = Types.UserId;
    type DeckId = Types.DeckId;
    type SlideId = Types.SlideId;

    type OwnerDeckBucket = DeckBucketTypes.OwnerDeckBucket;
    type ProtectedDeckBucket = DeckBucketTypes.ProtectedDeckBucket;
    type ProtectedDeckBuckets = DeckBucketTypes.ProtectedDeckBuckets;

    type DeckBucket = DeckBucket.DeckBucket;

    public class Store() {
        private var decks: HashMap.HashMap<UserId, HashMap.HashMap<DeckId, OwnerDeckBucket>> = HashMap.HashMap<UserId, HashMap.HashMap<DeckId, OwnerDeckBucket>>(10, Utils.isPrincipalEqual, Principal.hash);

        public func init(user: Principal, deckId: DeckId): async (ProtectedDeckBucket) {
            let deckBucket: ProtectedDeckBucket = getDeck(user, deckId);

            switch (deckBucket.error) {
                case (?error) {
                    return deckBucket;
                };
                case null {
                    switch (deckBucket.bucket) {
                        case (?bucket) {
                            return deckBucket;
                        };
                        case null {
                            let b: DeckBucket = await initNewBucket();

                            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeckBucket> = decks.get(user);

                            let newDeckBucket: OwnerDeckBucket = {
                                bucket = b;
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
                                bucket = ?b;
                                error = null;
                            };
                        };
                    }
                };
            };
        };

        // TODO: FIXME caller !== user -> owner as constructor variable?
        private func initNewBucket(): async (DeckBucket) {
            Cycles.add(1_000_000_000_000);
            let b: DeckBucket = await DeckBucket.DeckBucket();

            return b;
        };

        private func setOwnerDeck(user: Principal, deckId: DeckId, newDeckBucket: OwnerDeckBucket, ownerDecks: HashMap.HashMap<DeckId, OwnerDeckBucket>) {
            ownerDecks.put(deckId, newDeckBucket);
            decks.put(user, ownerDecks);
        };

        public func getDeck(user: Principal, deckId: DeckId): ProtectedDeckBucket {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeckBucket> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    let ownerDeck: ProtectedDeckBucket = getOwnerDeck(user, deckId, ownerDecks);
                    return ownerDeck;
                };
                case null {
                    return {
                        bucket = null;
                        error = null;
                    };
                };
            };
        };

        public func getDecks(user: Principal): ProtectedDeckBuckets {
            let ownerDecks: ?HashMap.HashMap<DeckId, OwnerDeckBucket> = decks.get(user);

            switch ownerDecks {
                case (?ownerDecks) {
                    var results: ([DeckBucket]) = [];

                    for ((deckId: DeckId, value: OwnerDeckBucket) in ownerDecks.entries()) {
                        if (Utils.isPrincipalNotEqual(user, value.owner)) {
                            return {
                                buckets = [];
                                error = ?"User does not have the permission for one of the deck.";
                            };
                        };

                        results := Array.append(results, [value.bucket]);
                    };

                    return {
                        buckets = results;
                        error = null;
                    };
                };
                case null {
                    return {
                        buckets = [];
                        error = null;
                    }
                }
            };
        };

        private func getOwnerDeck(user: Principal, deckId: DeckId, ownerDecks: HashMap.HashMap<DeckId, OwnerDeckBucket>): ProtectedDeckBucket {
            let ownerDeck: ?OwnerDeckBucket = ownerDecks.get(deckId);

            switch ownerDeck {
                case (?ownerDeck) {
                    if (Utils.isPrincipalEqual(user, ownerDeck.owner)) {
                        return {
                            bucket = ?ownerDeck.bucket;
                            error = null;
                        };
                    };
                };
                case null {
                    return {
                        bucket = null;
                        error = null;
                    };
                };
            };

            return {
                bucket = null;
                error = ?"User does not have the permission for the deck.";
            };
        };

        public func preupgrade(): HashMap.HashMap<Principal, [(DeckId, OwnerDeckBucket)]> {
            let entries : HashMap.HashMap<Principal, [(DeckId, OwnerDeckBucket)]> = HashMap.HashMap<Principal, [(DeckId, OwnerDeckBucket)]>(10, Utils.isPrincipalEqual, Principal.hash);

            for ((key: Principal, value: HashMap.HashMap<DeckId, OwnerDeckBucket>) in decks.entries()) {
                let ownerDecks : [(DeckId, OwnerDeckBucket)] = Iter.toArray<(DeckId, OwnerDeckBucket)>(value.entries());
                entries.put(key, ownerDecks);
            };

            return entries;
        };

        public func postupgrade(entries: [(Principal, [(DeckId, OwnerDeckBucket)])]) {
            for ((key: Principal, value: [(DeckId, OwnerDeckBucket)]) in entries.vals()) {
                let ownerDecks: HashMap.HashMap<DeckId, OwnerDeckBucket> = HashMap.fromIter<DeckId, OwnerDeckBucket>(Iter.fromArray<(DeckId, OwnerDeckBucket)>(value), 10, Text.equal, Text.hash);

                decks.put(key, ownerDecks);
            };
        };
    }

}
