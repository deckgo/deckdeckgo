with
{ pkgs = import ./nix {};
};
let
  pkg = pkgs.haskellPackages.developPackage
    { root = ./handler; };
in
  pkg.overrideAttrs(attr: {
    buildInputs = with pkgs; [ niv terraform awscli postgresql moreutils ];
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

       function start_pg() {
         ${pgutil.start_pg} || echo "PG start failed"
       }

       function stop_pg() {
         ${pgutil.stop_pg}
       }

       function repl_handler() {
          ghci handler/app/Test.hs handler/src/DeckGo/Handler.hs
       }

       function repl_unsplash() {
          ghci unsplash-proxy/Main.hs
       }

       function repl() {
          repl_handler
       }

      '';
   })
