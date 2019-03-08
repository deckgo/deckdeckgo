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

  test = pkgs.runCommand "run-tests" { buildInputs = [ pkgs.unzip pkgs.nodejs ]; }
    ''
      cd $(mktemp -d)
      unzip ${function}/function.zip
      node ${testMain}
      touch $out
    '';

  testMain = pkgs.writeText "main.js"
      ''
        'use strict'

        var m = require(process.cwd() + '/main.js');

        const request = {foo: "bar"};
        console.log("Sending request");
        m.handler(request, null, (foo, resp) => {
          console.log("Got response");
          if(resp.statusCode != 200) {
            throw "Expected status code to be 200!!!";
          }
          process.exit(0);
        });
        '';
}
