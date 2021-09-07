import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Time "mo:base/Time";
import Blob "mo:base/Blob";

import Types "../common/types";

module {

    type DeckId = Types.DeckId;

    public type Deck = {
        deckId: DeckId;

        data: Blob;

        created_at: Time.Time;
        updated_at: Time.Time;
    };

}
