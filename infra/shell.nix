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

      '';
   })
