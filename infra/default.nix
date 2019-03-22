with { pkgs = import ./nix {}; };

rec
{ function =
  pkgs.wai-lambda.wai-lambda-js-build-lambda "${handler}/bin/handler";

  handler = pkgs.haskellPackagesStatic.deckdeckgo-handler;

  dynamoJar = pkgs.runCommand "dynamodb-jar" { buildInputs = [ pkgs.gnutar ]; }
  ''
  mkdir -p $out
  cd $out
  tar -xvf ${pkgs.sources.dynamodb}
  '';

  test = pkgs.runCommand "tests" { buildInputs = [ pkgs.jre pkgs.curl pkgs.netcat pkgs.strace ]; }
  ''

      java -Djava.library.path=${dynamoJar}/DynamoDBLocal_lib -jar ${dynamoJar}/DynamoDBLocal.jar -sharedDb -port 8000 &

      while ! nc -z 127.0.0.1 8000; do
        echo waiting for DynamoDB
        sleep 1
      done
      sleep 2


      ls ${pkgs.otherport}/lib

      strace -f -e trace=network -s 10000 curl dynamodb.us-east-1.coo

      exit 1

      NIX_REDIRECTS=/etc/hosts=${hostsFile} \
        OLD_PORT=80 NEW_PORT=8000 \
        LD_PRELOAD="${pkgs.libredirect}/lib/libredirect.so ${pkgs.otherport}/lib/otherport.so" \
        AWS_DEFAULT_REGION=us-east-1 \
        AWS_ACCESS_KEY_ID=dummy \
        AWS_SECRET_ACCESS_KEY=dummy \
        strace curl dynamodb.us-east-1.amazonaws.com

      NIX_REDIRECTS=/etc/hosts=${hostsFile} \
        OLD_PORT=80 NEW_PORT=8000 \
        LD_PRELOAD="${pkgs.libredirect}/lib/libredirect.so ${pkgs.otherport}/lib/otherport.so" \
        AWS_DEFAULT_REGION=us-east-1 \
        AWS_ACCESS_KEY_ID=dummy \
        AWS_SECRET_ACCESS_KEY=dummy \
        ${handler}/bin/server &

      while ! nc -z 127.0.0.1 8080; do
        echo waiting for warp
        sleep 1
      done

      echo "Running tests"
      ${handler}/bin/test

      sleep 1

  '';

  hostsFile = pkgs.writeText "hosts"
  ''
    127.0.0.1 dynamodb.us-east-1.amazonaws.com
  '';
}
