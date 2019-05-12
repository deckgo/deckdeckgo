with { pkgs = import ./nix {}; };

rec
{ function =
    pkgs.runCommand "build-lambda" {}
      ''
        cp ${pkgs.wai-lambda.wai-lambda-js-wrapper} main.js
        # Can't be called 'main' otherwise lambda tries to load it
        cp "${handlerStatic}/bin/handler" main_hs
        cp ${./google-public-keys.json} google-public-keys.json
        mkdir $out
        ${pkgs.zip}/bin/zip -r $out/function.zip main.js main_hs google-public-keys.json
      '';

  handlerStatic = pkgs.haskellPackagesStatic.deckdeckgo-handler;
  handler = pkgs.haskellPackages.deckdeckgo-handler;

  dynamoJar = pkgs.runCommand "dynamodb-jar" { buildInputs = [ pkgs.gnutar ]; }
  ''
  mkdir -p $out
  cd $out
  tar -xvf ${pkgs.sources.dynamodb}
  '';

  publicKey = builtins.readFile ./public.cer;

  swaggerUi = pkgs.runCommand "swagger-ui" {}
  ''
    mkdir -p $out
    ${handler}/bin/swagger $out
  '';

  googleResp = { "key1" = publicKey ; };

  apiDir = pkgs.writeTextFile
      { name = "google-resp";
        destination = "/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com";
        text = builtins.toJSON googleResp;
      };

  # TODO: don't use latest dynamodb (but pin version)

  test = let
    pgutil = pkgs.callPackage ./pgutil.nix {};
    script = pkgs.writeScript "run-tests"
      ''
      #!${pkgs.stdenv.shell}
      set -euo pipefail

      # Set up DynamoDB
      java \
        -Djava.library.path=${dynamoJar}/DynamoDBLocal_lib \
        -jar ${dynamoJar}/DynamoDBLocal.jar \
        -sharedDb -port 8000 &

      while ! nc -z 127.0.0.1 8000; do
        echo waiting for DynamoDB
        sleep 1
      done

      export AWS_DEFAULT_REGION=us-east-1
      export AWS_ACCESS_KEY_ID=dummy
      export AWS_SECRET_ACCESS_KEY=dummy

      aws dynamodb create-table \
        --table-name Users \
        --attribute-definitions \
            AttributeName=UserFirebaseId,AttributeType=S \
        --key-schema AttributeName=UserFirebaseId,KeyType=HASH \
        --endpoint-url http://127.0.0.1:8000 \
        --provisioned-throughput ReadCapacityUnits=1,WriteCapacityUnits=1 \
        > /dev/null

      aws dynamodb create-table \
        --table-name Decks \
        --attribute-definitions \
            AttributeName=DeckId,AttributeType=S \
        --key-schema AttributeName=DeckId,KeyType=HASH \
        --endpoint-url http://127.0.0.1:8000 \
        --provisioned-throughput ReadCapacityUnits=1,WriteCapacityUnits=1 \
        > /dev/null

      aws dynamodb create-table \
        --table-name Slides \
        --attribute-definitions \
            AttributeName=SlideId,AttributeType=S \
        --key-schema AttributeName=SlideId,KeyType=HASH \
        --endpoint-url http://127.0.0.1:8000 \
        --provisioned-throughput ReadCapacityUnits=1,WriteCapacityUnits=1 \
        > /dev/null

      export PGHOST=localhost
      export PGPORT=5432
      export PGDATABASE=test_db
      export PGUSER=test_user
      export PGPASSWORD=test_pass

      ${pgutil.start_pg}

      echo "Running tests"
      NIX_REDIRECTS=/etc/protocols=${pkgs.iana-etc}/etc/protocols \
        LD_PRELOAD="${pkgs.libredirect}/lib/libredirect.so" \
        GOOGLE_PUBLIC_KEYS="${pkgs.writeText "google-x509" (builtins.toJSON googleResp)}" \
        FIREBASE_PROJECT_ID="my-project-id" \
        TEST_TOKEN_PATH=${./token} ${handler}/bin/test
      echo "Tests were run"

      touch $out
      '';
    in pkgs.runCommand "tests"
    { buildInputs =
        [ pkgs.jre
          pkgs.netcat
          pkgs.awscli
          pkgs.haskellPackages.wai-app-static
          pkgs.postgresql
          pkgs.moreutils
        ];
    } script;
}
