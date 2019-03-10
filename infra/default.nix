with { pkgs = import ./nix {}; };

# TODO:
#  - plug DynamoDBLocal in tests
#       -> https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.html

rec
{ function = pkgs.runCommand "build-function" {}
  ''
    cp ${./main.js} main.js
    # Can't be called 'main' otherwise lambda tries to load it
    cp ${handler}/bin/deckdeckgo-handler main_hs
    mkdir $out
    ${pkgs.zip}/bin/zip -r $out/function.zip main.js main_hs
  '';

  handler = pkgs.haskellPackagesStatic.deckdeckgo-handler;
  handler-client = pkgs.haskellPackages.deckdeckgo-handler-client;
  handler-api = pkgs.haskellPackages.deckdeckgo-handler-api;
  wai-lambda = pkgs.haskellPackages.wai-lambda;
}
