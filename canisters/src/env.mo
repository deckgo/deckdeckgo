module {

    public type EnvSecrets = {
        manager: Text;
    };

    public let secrets: EnvSecrets = {
        manager = "hello";
    };

}
