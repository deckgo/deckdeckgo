with { pkgs = import ./nix {}; };

rec
{ function = # TODO: rename to handler
    pkgs.runCommand "build-lambda" {}
      ''
        cp ${pkgs.wai-lambda.wai-lambda-js-wrapper} main.js
        # Can't be called 'main' otherwise lambda tries to load it
        cp "${handlerStatic}/bin/handler" main_hs
        cp ${./google-public-keys.json} google-public-keys.json
        mkdir $out
        ${pkgs.zip}/bin/zip \
          -r $out/function.zip \
          main.js main_hs google-public-keys.json
      '';

  # TODO: move all other builders to this
  function-handler-path =
    { path = builtins.seq
        (builtins.readDir function) "${function}/function.zip";
    } ;

  function-unsplash =
    pkgs.runCommand "build-lambda" {}
      ''
        cp ${pkgs.wai-lambda.wai-lambda-js-wrapper} main.js
        # Can't be called 'main' otherwise lambda tries to load it
        cp "${unsplashProxyStatic}/bin/unsplash-proxy" main_hs
        mkdir $out
        ${pkgs.zip}/bin/zip -r $out/function.zip main.js main_hs
      '';

  function-presenter-path =
    { path = builtins.seq
        (builtins.readDir function-presenter) "${function-presenter}/function.zip";
    } ;

  function-presenter =
    pkgs.runCommand "build-lambda-presenter" {}
      ''
        cp ${pkgs.wai-lambda.wai-lambda-js-wrapper} main.js
        # Can't be called 'main' otherwise lambda tries to load it
        cp "${handlerStatic}/bin/presenter" main_hs
        cp ${deckdeckgo-starter-dist}/dist.tar dist.tar
        mkdir -p $out
        ${pkgs.zip}/bin/zip -r $out/function.zip main.js main_hs dist.tar
      '';

  deckdeckgo-starter-dist =
    with
      { napalm = import pkgs.sources.napalm { inherit pkgs;} ; };
    pkgs.runCommand "deckdeckgo-starter" { buildInputs = [ pkgs.nodejs-10_x ]; }
      ''
        cp -r ${napalm.buildPackage pkgs.sources.deckdeckgo-starter {}}/* .
        chmod +w -R _napalm-install
        cd _napalm-install
        patchShebangs node_modules/webpack/bin/webpack.js
        npm run build
        mkdir -p $out
        pushd dist
        tar -cvf $out/dist.tar *
        popd
      '';

  devshell = if ! pkgs.lib.inNixShell then null else
    with
      { pkg = pkgs.haskellPackages.developPackage { root = ./handler; } ; };
    pkg.overrideAttrs(attr: {
      buildInputs = with pkgs;
        [ niv terraform awscli postgresql moreutils minio ];
      shellHook =
      let
        pgutil = pkgs.callPackage ./pgutil.nix {};
      in
        ''
         function load_pg() {
          export PGHOST=localhost
          export PGPORT=5432
          export PGDATABASE=test_db
          export PGUSER=test_user
          export PGPASSWORD=test_pass
         }

         function start_services() {
           load_pg
           ${pgutil.start_pg} || echo "PG start failed"
           if [ ! -f .dynamodb.pid ]; then
            echo "Starting dynamodb"
            java \
              -Djava.library.path=${dynamoJar}/DynamoDBLocal_lib \
              -jar ${dynamoJar}/DynamoDBLocal.jar \
              -sharedDb -port 8123 &
            echo $! > .dynamodb.pid
           else
            echo "Looks like dynamo is already running"
           fi
           if [ ! -f .sqs.pid ]; then
            echo "Starting SQS"
            java \
              -jar ${pkgs.sources.elasticmq} &
            echo $! > .sqs.pid
           else
            echo "Looks like SQS is already running"
           fi

           if [ ! -f .s3.pid ]; then
            echo "Starting S3"
            MINIO_ACCESS_KEY=dummy \
              MINIO_SECRET_KEY=dummy_key \
              minio server --address localhost:9000 $(mktemp -d) &
            echo $! > .s3.pid
           else
            echo "Looks like S3 is already running"
           fi
           export GOOGLE_PUBLIC_KEYS="${pkgs.writeText "google-x509" (builtins.toJSON googleResp)}"
           export FIREBASE_PROJECT_ID="my-project-id"
           export TEST_TOKEN_PATH=${./token}
         }

         function stop_services() {
           ${pgutil.stop_pg}
           if [ -f .dynamodb.pid ]; then
            echo "Killing dynamodb"
            kill $(cat .dynamodb.pid)
            rm .dynamodb.pid
           else
            echo "Looks like dynamodb is not running"
           fi
           if [ -f .sqs.pid ]; then
            echo "Killing SQS"
            kill $(cat .sqs.pid)
            rm .sqs.pid
           else
            echo "Looks like SQS is not running"
           fi
           if [ -f .s3.pid ]; then
            echo "Killing S3"
            kill $(cat .s3.pid)
            rm .s3.pid
           else
            echo "Looks like S3 is not running"
           fi
           rm -rf .pgdata
           rm shared-local-instance.db
         }

         function repl_handler() {
            AWS_DEFAULT_REGION=us-east-1 \
              AWS_ACCESS_KEY_ID=dummy \
              AWS_SECRET_ACCESS_KEY=dummy_key \
              DECKGO_STARTER_DIST=${deckdeckgo-starter-dist}/dist.tar \
              ghci -Wall handler/app/Test.hs handler/src/DeckGo/*.hs
         }

         function repl_unsplash() {
            ghci -Wall unsplash-proxy/Main.hs
         }

         function repl() {
            repl_handler
         }

        '';
     });

  handlerStatic = pkgs.haskellPackagesStatic.deckdeckgo-handler;
  handler = pkgs.haskellPackages.deckdeckgo-handler;

  unsplashProxyStatic = pkgs.haskellPackagesStatic.unsplash-proxy;
  unsplashProxy = pkgs.haskellPackages.unsplash-proxy;

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

  test = let
    pgutil = pkgs.callPackage ./pgutil.nix {};
    script = pkgs.writeScript "run-tests"
      ''
      #!${pkgs.stdenv.shell}
      set -euo pipefail

      export AWS_DEFAULT_REGION=us-east-1
      export AWS_ACCESS_KEY_ID=dummy
      export AWS_SECRET_ACCESS_KEY=dummy_key

      # Set up DynamoDB
      java \
        -Djava.library.path=${dynamoJar}/DynamoDBLocal_lib \
        -jar ${dynamoJar}/DynamoDBLocal.jar \
        -sharedDb -port 8123 &

      java \
        -jar ${pkgs.sources.elasticmq} &

      MINIO_ACCESS_KEY=dummy \
        MINIO_SECRET_KEY=dummy_key \
        minio server --address localhost:9000 $(mktemp -d) &

      while ! nc -z 127.0.0.1 8123; do
        echo waiting for DynamoDB
        sleep 1
      done

      while ! nc -z 127.0.0.1 9324; do
        echo waiting for SQS
        sleep 1
      done

      while ! nc -z 127.0.0.1 9000; do
        echo waiting for S3
        sleep 1
      done

      aws s3api create-bucket --bucket deckgo-bucket \
          --endpoint-url http://127.0.0.1:9000

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
        FIREBASE_PROJECT_ID="my-project-id" \
        AWS_DEFAULT_REGION=us-east-1 \
        AWS_ACCESS_KEY_ID=dummy \
        AWS_SECRET_ACCESS_KEY=dummy_key \
        DECKGO_STARTER_DIST=${deckdeckgo-starter-dist}/dist.tar \
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
          pkgs.minio
        ];
      LANG = "en_US.UTF-8";
      LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    } script;
}
