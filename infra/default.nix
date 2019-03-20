with { pkgs = import ./nix {}; };

# TODO:
#  - plug DynamoDBLocal in tests
#       -> https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.html

rec
{ function =
    pkgs.wai-lambda.wai-lambda-js-build-lambda "${handler}/bin/deckdeckgo-handler";

  handler = pkgs.haskellPackagesStatic.deckdeckgo-handler;
}
