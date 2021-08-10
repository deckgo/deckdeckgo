import Option "mo:base/Option";

import Error "mo:base/Error";

import Types "../common/types";
import DeckDataTypes "./deck.data.types";

import Utils "../common/utils";

// import Slides "canister:slides";

actor class DeckBucket() {

  type UserId = Types.UserId;
  type DeckId = Types.DeckId;
  type SlideId = Types.SlideId;

  type Deck = DeckDataTypes.Deck;

  var owner: ?UserId = null;
  var deck: ?Deck = null;

  /**
   * TODO: Should we also secure the deckId ?
   */

  public shared query({ caller }) func get() : async Deck {
    switch owner {
        case (?owner) {
            if (Utils.isPrincipalNotEqual(caller, owner)) {
                throw Error.reject("User does not have the permission to get the deck.");
            };
        };
        case null {};
    };

    switch (deck) {
        case (?deck) {
            return deck;
        };
        case null {
            throw Error.reject("Deck not found.");
        };
    };
  };

  public shared({ caller }) func set(newDeck: Deck) : async () {
    switch owner {
        case (?owner) {
            if (Utils.isPrincipalNotEqual(caller, owner)) {
                throw Error.reject("User does not have the permission to set the deck.");
            };
        };
        case null {
            owner := ?caller;
        };
    };

    deck := ?newDeck;
  };

  public shared({ caller }) func del(slides: Bool) : async (Bool) {
    await deleteSlides(caller);

    let exists: Bool = Option.isSome(deck);
    if (exists) {
        deck := null;
        owner := null;
    };

    return exists;

    // TODO: what to do more to destroy?
  };

  private func deleteSlides(user: Principal): async () {
    switch owner {
        case (?owner) {
            if (Utils.isPrincipalNotEqual(user, owner)) {
                throw Error.reject("User does not have the permission to delete the deck.");
            };
        };
        case null {};
    };

    switch (deck) {
        case (?deck) {
            let slides: ?[SlideId] = deck.data.slides;

            switch (slides) {
                case (?slides) {
                    for ((slideId: Text) in slides.vals()) {
                        // TODO: reactivate delete slide
                        // let slideExists: Bool = await Slides.delAdmin(user, slideId);
                    };
                };
                case null {}
            };
        };
        case null {
            throw Error.reject("Deck not defined.");
        };
    };
  };

};
