{ stdenv
, coreutils
, glibcLocales
, gnugrep
, lib
, postgresql
, writeScript
}: act:

let
  path = lib.makeBinPath [
    coreutils
    glibcLocales
    gnugrep
    postgresql
  ];
in writeScript "with-pg" ''
    #!${stdenv.shell}
    set -euo pipefail
    export PATH=${path}:$PATH
    export HOME=$(mktemp -d tests-home.XXXXX)
    # TODO: gather PG logs?

    TRAPS="echo all traps executed"
    traps() {
      TRAPS="echo killing $1 && ($2 || true) && echo killed && $TRAPS"
      trap "$TRAPS" EXIT
    }

    ## Setup postgresql
    PGDATA=$(mktemp -d postgres.XXXXX)

    # export everything for the sub script
    export POSTGRES_DB=test_user
    export POSTGRES_USER=test_user
    export POSTGRES_PASSWORD=supersecret

    echo "Initializing postgres DB"
    initdb "$PGDATA"

    echo "Starting postgres DB"
    pg_ctl -D "$PGDATA" -w start
    # nix waits for all processes to exit
    traps postgresql "pg_ctl -D \"$PGDATA\" -w -m immediate stop >/dev/null" EXIT
    echo "Creating PG tables"
    psql=( psql -X -w -v ON_ERROR_STOP=1 )
    "''${psql[@]}" -d postgres <<-EOSQL
      CREATE DATABASE "$POSTGRES_DB" ;
    EOSQL
    "''${psql[@]}" -d postgres <<-EOSQL
      CREATE USER "$POSTGRES_USER" WITH SUPERUSER PASSWORD '$POSTGRES_PASSWORD';
    EOSQL

    echo "Running with-pg user script"
    ${act}

    echo "Done."
  ''
