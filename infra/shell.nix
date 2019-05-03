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
         script = pkgs.writeScript "start-pg"
          ''
        #!/usr/bin/env bash
        pg_pid=""
        set -euo pipefail
        export PGDATA=$(mktemp -d)
        echo data dir is "$PGDATA"
        mkdir -p $PGDATA
        chmod 0700 -R $PGDATA
        export PGPORT=5432

        initdb
        cat << EOF > $PGDATA/pg_hba.conf
        local all $USER ident
        local all all   password
        EOF

        postgres 2>&1 | ts '[%Y-%m-%d %H:%M:%S]' > $PGDATA/pglog &

        until psql postgres -c "SELECT 1" > /dev/null 2>&1 ; do
            echo waiting for pg
            sleep 0.5
        done

        psql postgres -c "CREATE DATABASE $USER"

        pg_pid=$!
        echo pg_pid is "$pg_pid"
          '';
      in
      ''
        # makes sure Ctrl-C doesn't kill PG
        (set -m; ${script} || echo "PG start failed")

      '';
   })
