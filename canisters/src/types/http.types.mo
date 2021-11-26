import Text "mo:base/Text";
import Nat8 "mo:base/Nat8";
import Nat16 "mo:base/Nat16";

module {
    public type HeaderField = (Text, Text);

    public type HttpRequest = {
        url : Text;
        method : Text;
        body : [Nat8];
        headers : [HeaderField];
    };

    public type HttpResponse = {
        body : [Nat8];
        headers : [HeaderField];
        status_code : Nat16;
        streaming_strategy : ?StreamingStrategy;
    };

    public type StreamingStrategy = {
        #Callback : {
            token : StreamingCallbackToken;
            callback : shared () -> async ();
        };
    };

    public type StreamingCallbackToken = {
        fullPath: Text;
        token: ?Text;
        contentEncoding : Text;

        index : Nat;

        // TODO: do we need sha256 ?
        // sha256 : ?[Nat8];
    };

    public type StreamingCallbackHttpResponse = {
        body : [Nat8];
        token: ?StreamingCallbackToken;
    };
}
