import Text "mo:base/Text";
import Time "mo:base/Time";
import Blob "mo:base/Blob";

module {

    public type Data = {
        id: Text;

        data: Blob;

        created_at: Time.Time;
        updated_at: Time.Time;
    };

}
