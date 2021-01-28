with { pkgs = import ./nix {}; };

rec
{ 
  function = # TODO: rename to handler
    pkgs.runCommand "build-lambda" {}
      ''
        cp ${./main.js} main.js
        # Can't be called 'main' otherwise lambda tries to load it
        cp "${handler}/bin/handler" main_hs
        cp ${deckdeckgo-starter-dist}/dist.zip dist.zip
        mkdir $out
        ${pkgs.zip}/bin/zip -r $out/function.zip main.js main_hs dist.zip
      '';

  # TODO: move all other builders to this
  function-handler-path =
    { path = builtins.seq
        (builtins.readDir function) "${function}/function.zip";
    } ;


  function-unsplash =
    pkgs.runCommand "build-lambda" {}
      ''
        cp ${./main.js} main.js
        # Can't be called 'main' otherwise lambda tries to load it
        cp "${unsplashProxy}/bin/unsplash-proxy" main_hs
        mkdir $out
        ${pkgs.zip}/bin/zip -r $out/function.zip main.js main_hs
      '';

  function-google-key-updater-path =
    { path = builtins.seq
        (builtins.readDir function-google-key-updater) "${function-google-key-updater}/function.zip";
    } ;

  function-google-key-updater =
    pkgs.runCommand "build-lambda" {}
      ''
        cp ${./main.js} main.js
        # Can't be called 'main' otherwise lambda tries to load it
        cp "${googleKeyUpdater}/bin/google-key-updater" main_hs
        mkdir $out
        ${pkgs.zip}/bin/zip -r $out/function.zip main.js main_hs
      '';

  function-dirty-path =
    { path = builtins.seq
        (builtins.readDir function-dirty) "${function-dirty}/function.zip";
    } ;

  function-dirty =
    pkgs.runCommand "build-lambda-dirty" {}
      ''
        cp ${./main.js} main.js
        # Can't be called 'main' otherwise lambda tries to load it
        cp "${handler}/bin/dirty" main_hs
        mkdir -p $out
        ${pkgs.zip}/bin/zip -r $out/function.zip main.js main_hs
      '';

  deckdeckgo-starter-dist =
    with
      { napalm = import pkgs.sources.napalm { inherit pkgs;} ; };
    pkgs.runCommand "deckdeckgo-starter" { buildInputs = [ pkgs.nodejs pkgs.zip ]; }
      ''
        cp -r ${napalm.buildPackage pkgs.sources.deckdeckgo-starter {}}/* .
        chmod +w -R _napalm-install
        cd _napalm-install
        patchShebangs node_modules/webpack/bin/webpack.js
        npm run build
        mkdir -p $out
        pushd dist
        zip -r $out/dist.zip *
        popd
      '';


  devshell = handler.env;
  #devshell = if ! pkgs.lib.inNixShell then null else
    #with
      #{ pkg = pkgs.haskellPackages.developPackage { root = ./handler; } ; };
    #pkg.overrideAttrs(attr: {
      #buildInputs = with pkgs;
      #[];
        ##[ terraform awscli postgresql moreutils minio ];
      #LANG = "en_US.UTF-8";
      #LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      #shellHook =
      #let
        #pgutil = pkgs.callPackage ./pgutil.nix {};
      #in
        #''
         #function load_pg() {
          #export PGHOST=localhost
          #export PGPORT=5432
          #export PGDATABASE=test_db
          #export PGUSER=test_user
          #export PGPASSWORD=test_pass
         #}

         #function start_services() {
           #load_pg
           #${pgutil.start_pg} || echo "PG start failed"
           #if [ ! -f .sqs.pid ]; then
            #echo "Starting SQS"
            #java \
              #-jar ${pkgs.sources.elasticmq} &
            #echo $! > .sqs.pid
           #else
            #echo "Looks like SQS is already running"
           #fi

           #if [ ! -f .s3.pid ]; then
            #echo "Starting S3"
            #MINIO_ACCESS_KEY=dummy \
              #MINIO_SECRET_KEY=dummy_key \
              #minio server --address localhost:9000 $(mktemp -d) &
            #echo $! > .s3.pid
           #else
            #echo "Looks like S3 is already running"
           #fi
           #export GOOGLE_PUBLIC_KEYS="${pkgs.writeText "google-x509" (builtins.toJSON googleResp)}"
           #export FIREBASE_PROJECT_ID="my-project-id"
           #export TEST_TOKEN_PATH=${./token}
         #}

         #function stop_services() {
           #${pgutil.stop_pg}
           #if [ -f .sqs.pid ]; then
            #echo "Killing SQS"
            #kill $(cat .sqs.pid)
            #rm .sqs.pid
           #else
            #echo "Looks like SQS is not running"
           #fi
           #if [ -f .s3.pid ]; then
            #echo "Killing S3"
            #kill $(cat .s3.pid)
            #rm .s3.pid
           #else
            #echo "Looks like S3 is not running"
           #fi
           #rm -rf .pgdata
           #rm shared-local-instance.db
         #}

         #function repl_handler() {
            #shopt -s globstar
            #AWS_DEFAULT_REGION=us-east-1 \
              #AWS_ACCESS_KEY_ID=dummy \
              #AWS_SECRET_ACCESS_KEY=dummy_key \
              #DECKGO_STARTER_DIST=${deckdeckgo-starter-dist}/dist.zip \
              #ghci -Wall handler/app/Test.hs handler/src/**/*.hs
         #}

         #function repl_unsplash() {
            #ghci -Wall unsplash-proxy/Main.hs
         #}

         #function repl_google_key_updater() {
            #ghci -Wall google-key-updater/Main.hs
         #}

         #function repl() {
            #repl_handler
         #}

        #'';
     #});

  handler = pkgs.haskellPackages.deckdeckgo-handler;

  unsplashProxy = pkgs.haskellPackages.unsplash-proxy;

  googleKeyUpdater = pkgs.haskellPackages.google-key-updater;

  publicKey = builtins.readFile ./public.cer;

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

      java \
        -jar ${pkgs.sources.elasticmq} &

      MINIO_ACCESS_KEY=dummy \
        MINIO_SECRET_KEY=dummy_key \
        minio server --address localhost:9000 $(mktemp -d) &

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
      GOOGLE_PUBLIC_KEYS="${pkgs.writeText "google-x509" (builtins.toJSON googleResp)}" \
        FIREBASE_PROJECT_ID="my-project-id" \
        FIREBASE_PROJECT_ID="my-project-id" \
        AWS_DEFAULT_REGION=us-east-1 \
        AWS_ACCESS_KEY_ID=dummy \
        AWS_SECRET_ACCESS_KEY=dummy_key \
        DECKGO_STARTER_DIST=${deckdeckgo-starter-dist}/dist.zip \
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
