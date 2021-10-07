import Option "mo:base/Option";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Iter "mo:base/Iter";
import Blob "mo:base/Blob";

import Error "mo:base/Error";

import Types "../types/types";
import DeckDataTypes "./deck.data.types";
import SlideDataTypes "./slide.data.types";

import Utils "../types/utils";

import WalletUtils "../utils/wallet.utils";

actor class DeckBucket(owner: Types.UserId) = this {

  type UserId = Types.UserId;
  type DeckId = Types.DeckId;
  type SlideId = Types.SlideId;

  type Deck = DeckDataTypes.Deck;
  type Slide = SlideDataTypes.Slide;

  private stable let user: Types.UserId = owner;

  private stable var deck: ?Deck = null;

  // Preserve the application state on upgrades
  private stable var entries : [(SlideId, Slide)] = [];

  private var slides: HashMap.HashMap<SlideId, Slide> = HashMap.HashMap<SlideId, Slide>(10, Text.equal, Text.hash);

  private let walletUtils: WalletUtils.WalletUtils = WalletUtils.WalletUtils();

   /**
    * Deck
    */

  public shared query({ caller }) func get() : async Deck {
    if (Utils.isPrincipalNotEqual(caller, user)) {
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
    if (Utils.isPrincipalNotEqual(caller, user)) {
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

  /**
   * Slides
   */

  public shared query({ caller }) func getSlide(slideId: SlideId) : async Slide {
    if (Utils.isPrincipalNotEqual(caller, user)) {
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
    if (Utils.isPrincipalNotEqual(caller, user)) {
        throw Error.reject("User does not have the permission to set the slide.");
    };

    slides.put(slide.slideId, slide);
  };

  public shared({ caller }) func delSlide(slideId: SlideId) : async (Bool) {
    if (Utils.isPrincipalNotEqual(caller, user)) {
        throw Error.reject("User does not have the permission to delete the slide.");
    };

    let slide: ?Slide = slides.remove(slideId);

    let exists: Bool = Option.isSome(slide);
    return exists;
  };

  /**
   * Canister mgmt
   */

  // TODO: inter-canister call secure caller === manager canister !!!
  // Or as only controllers can execute following is enough security?

  public shared({ caller }) func transferCycles(): async() {
      await walletUtils.transferCycles(caller);
  };

  system func preupgrade() {
    entries := Iter.toArray(slides.entries());
  };

  system func postupgrade() {
    slides := HashMap.fromIter<SlideId, Slide>(entries.vals(), 10, Text.equal, Text.hash);
    entries := [];
  };

};
