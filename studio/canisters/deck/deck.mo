import Option "mo:base/Option";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Iter "mo:base/Iter";

import Error "mo:base/Error";

import Types "../common/types";
import DeckDataTypes "./deck.data.types";
import SlideDataTypes "./slide.data.types";

import Utils "../common/utils";

actor class DeckBucket(owner: Types.UserId) = this {

  type UserId = Types.UserId;
  type DeckId = Types.DeckId;
  type SlideId = Types.SlideId;

  type Deck = DeckDataTypes.Deck;
  type Slide = SlideDataTypes.Slide;

  private stable var deck: ?Deck = null;

  // Preserve the application state on upgrades
  private stable var entries : [(SlideId, Slide)] = [];

  private var slides: HashMap.HashMap<SlideId, Slide> = HashMap.HashMap<SlideId, Slide>(10, Text.equal, Text.hash);

   /**
    * Deck
    */

  public shared query({ caller }) func get() : async Deck {
    if (Utils.isPrincipalNotEqual(caller, owner)) {
        throw Error.reject("User does not have the permission to get the deck.");
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
    if (Utils.isPrincipalNotEqual(caller, owner)) {
        throw Error.reject("User does not have the permission to set the deck.");
    };

    switch (deck) {
      case (?deck) {
        if (deck.deckId != newDeck.deckId) {
          throw Error.reject("The provided Deck ID does not match the one of this deck.");
        };
      };
      case null {};
    };

    deck := ?newDeck;
  };

  // TODO: do we need to delete or destroy canister is enough?

  public shared({ caller }) func del() : async (Bool) {
    if (Utils.isPrincipalNotEqual(caller, owner)) {
        throw Error.reject("User does not have the permission to delete the deck.");
    };

    let exists: Bool = delete();

    return exists;
  };

  // TODO: only callable from user canister
  // TODO: if we have this security then we probably do not need to check the user as parameter

  public shared({ caller }) func delAdmin(user: UserId) : async (Bool) {
    if (Utils.isPrincipalNotEqual(user, owner)) {
        throw Error.reject("User does not match the owner of the deck to delete.");
    };

    let exists: Bool = delete();

    return exists;
  };

  private func delete(): Bool {
    slides := HashMap.fromIter<SlideId, Slide>([].vals(), 10, Text.equal, Text.hash);

    let exists: Bool = Option.isSome(deck);
    if (exists) {
        deck := null;
    };

    return exists;
  };

  /**
   * Slides
   */

  public shared query({ caller }) func getSlide(slideId: SlideId) : async Slide {
    if (Utils.isPrincipalNotEqual(caller, owner)) {
        throw Error.reject("User does not have the permission to get the slide.");
    };

    let slide: ?Slide = slides.get(slideId);

    switch (slide) {
        case (?slide) {
            return slide;
        };
        case null {
            throw Error.reject("Slide not found.");
        };
    };
  };

  public shared({ caller }) func setSlide(slide: Slide) : async () {
    if (Utils.isPrincipalNotEqual(caller, owner)) {
        throw Error.reject("User does not have the permission to set the slide.");
    };

    slides.put(slide.slideId, slide);
  };

  public shared({ caller }) func delSlide(slideId: SlideId) : async (Bool) {
    if (Utils.isPrincipalNotEqual(caller, owner)) {
        throw Error.reject("User does not have the permission to delete the slide.");
    };

    let slide: ?Slide = slides.remove(slideId);

    let exists: Bool = Option.isSome(slide);
    return exists;
  };

  /**
   * Canister
   */

  public query func id() : async Principal {
    return Principal.fromActor(this);
  };

  system func preupgrade() {
    entries := Iter.toArray(slides.entries());
  };

  system func postupgrade() {
    slides := HashMap.fromIter<SlideId, Slide>(entries.vals(), 10, Text.equal, Text.hash);
    entries := [];
  };

};
