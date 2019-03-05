with { pkgs = import ./nix {}; };

# TODO:
#  - split servant API into lib
#  - write Aeson request decoder
#  - write Aeson response decoder
#  - write JS request encoder
#  - write JS response decoder
#  - run node server to forward requests to 'handler'
#  - write (non-static) servant client using API lib
#  - plug DynamoDBLocal in tests
#       -> https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.html
#  - write async request handler in JS with file dump for response

rec
{ function = pkgs.runCommand "build-function" {}
  ''
    cp ${./main.js} main.js
    # Can't be called 'main' otherwise lambda tries to load it
    cp ${main}/bin/deckdeckgo-handler main_hs
    mkdir $out
    ${pkgs.zip}/bin/zip -r $out/function.zip main.js main_hs
  '';

  main = pkgs.haskellPackagesStatic.deckdeckgo-handler;

  test = pkgs.runCommand "foo" { buildInputs = [ pkgs.unzip pkgs.nodejs ]; }
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
        m.handler(request, null, (foo, resp) => {
          if(resp.statusCode != 200) {
            throw "Expected status code to be 200!!!";
          }
        });
        '';
}
