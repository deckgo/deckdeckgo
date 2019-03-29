with { pkgs = import ./nix {}; };

rec
{ function =
  pkgs.wai-lambda.wai-lambda-js-build-lambda "${handlerStatic}/bin/handler";

  handlerStatic = pkgs.haskellPackagesStatic.deckdeckgo-handler;
  handler = pkgs.haskellPackages.deckdeckgo-handler;

  dynamoJar = pkgs.runCommand "dynamodb-jar" { buildInputs = [ pkgs.gnutar ]; }
  ''
  mkdir -p $out
  cd $out
  tar -xvf ${pkgs.sources.dynamodb}
  '';

  publicKey = builtins.readFile ./public.cer;

  googleResp = { "1" = publicKey ; };

  apiDir = pkgs.writeTextFile
      { name = "google-resp";
        destination = "/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com";
        text = builtins.toJSON googleResp;
      };

  # TODO: don't use latest dynamodb (but pin version)

  test = pkgs.runCommand "tests" { buildInputs = [ pkgs.jre pkgs.curl pkgs.netcat pkgs.awscli pkgs.haskellPackages.wai-app-static]; }
  ''

      java -Djava.library.path=${dynamoJar}/DynamoDBLocal_lib -jar ${dynamoJar}/DynamoDBLocal.jar -sharedDb -port 8000 &

      while ! nc -z 127.0.0.1 8000; do
        echo waiting for DynamoDB
        sleep 1
      done
      export AWS_DEFAULT_REGION=us-east-1
      export AWS_ACCESS_KEY_ID=dummy
      export AWS_SECRET_ACCESS_KEY=dummy

      aws dynamodb create-table \
        --table-name Decks \
        --attribute-definitions \
            AttributeName=DeckId,AttributeType=S \
        --key-schema AttributeName=DeckId,KeyType=HASH \
        --endpoint-url http://127.0.0.1:8000 \
        --provisioned-throughput ReadCapacityUnits=1,WriteCapacityUnits=1

      aws dynamodb create-table \
        --table-name Slides \
        --attribute-definitions \
            AttributeName=SlideId,AttributeType=S \
        --key-schema AttributeName=SlideId,KeyType=HASH \
        --endpoint-url http://127.0.0.1:8000 \
        --provisioned-throughput ReadCapacityUnits=1,WriteCapacityUnits=1

      NIX_REDIRECTS=/etc/protocols=${pkgs.iana-etc}/etc/protocols \
        LD_PRELOAD="${pkgs.libredirect}/lib/libredirect.so" \
        ${handler}/bin/server &


      cp ${pkgs.writeText "foo" (builtins.toJSON googleResp)} cert
      while ! nc -z 127.0.0.1 8080; do
        echo waiting for server
        sleep 1
      done

      warp -d ${apiDir} -p 8081 &

      while ! nc -z 127.0.0.1 8081; do
        echo waiting for warp
        sleep 1
      done

      curl localhost:8081/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com

      echo "Running tests"
      ${handler}/bin/test ${./token}

      touch $out
  '';
}
