{ writeScript, stdenv }:
{
  start_pg = writeScript "start-pg"
    ''
      #!${stdenv.shell}
      pg_pid=""
      set -euo pipefail

      # TODO: explain what's happening here
      LOCAL_PGHOST=$PGHOST
      LOCAL_PGPORT=$PGPORT
      LOCAL_PGDATABASE=$PGDATABASE
      LOCAL_PGUSER=$PGUSER
      LOCAL_PGPASSWORD=$PGPASSWORD

      unset PGUSER PGPASSWORD

      initdb -D .pgdata

      echo "unix_socket_directories = '$(mktemp -d)'" >> .pgdata/postgresql.conf

      # TODO: port
      pg_ctl -D ".pgdata" -w start || (echo pg_ctl failed; exit 1)

      until psql postgres -c "SELECT 1" > /dev/null 2>&1 ; do
          echo waiting for pg
          sleep 0.5
      done

      psql postgres -w -c "CREATE DATABASE $LOCAL_PGDATABASE"
      psql postgres -w -c "CREATE ROLE $LOCAL_PGUSER WITH LOGIN PASSWORD '$LOCAL_PGPASSWORD'"
      psql postgres -w -c "GRANT ALL PRIVILEGES ON DATABASE $LOCAL_PGDATABASE TO $LOCAL_PGUSER"
    '';
  stop_pg = writeScript "stop-pg"
    ''
      #!${stdenv.shell}
      pg_pid=""
      set -euo pipefail

      pg_ctl -D .pgdata -w -m immediate stop
    '';
}
