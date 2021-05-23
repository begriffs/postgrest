{ bashCompletion
, buildToolbox
, checkedShellScript
, lib
, postgresqlVersions
, writeTextFile
}:
let
  withTmpDb =
    { name, postgresql }:
    checkedShellScript
      {
        name = "postgrest-with-${name}";
        docs = "Run the given command in a temporary database with ${name}";
        args =
          [
            "ARG_OPTIONAL_SINGLE([fixtures], [f], [SQL file to load fixtures from], [test/fixtures/load.sql])"
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
            "ARG_USE_ENV([PGUSER], [postgrest_test_authenticator], [Authenticator PG role])"
            "ARG_USE_ENV([PGDATABASE], [postgres], [PG database name])"
            "ARG_USE_ENV([PGRST_DB_SCHEMAS], [test], [Schema to expose])"
            "ARG_USE_ENV([PGRST_DB_ANON_ROLE], [postgrest_test_anonymous], [Anonymous PG role])"
          ];
        addCommandCompletion = true;
        inRootDir = true;
        redirectTixFiles = false;
        withPath = [ postgresql ];
        withTmpDir = true;
      }
      ''
        # avoid starting multiple layers of withTmpDb
        if test -v PGRST_DB_URI; then
          exec "$@"
        fi

        setuplog="$tmpdir/setup.log"

        log () {
          echo "$1" >> "$setuplog"
        }

        mkdir -p "$tmpdir"/{db,socket}
        # remove data dir, even if we keep tmpdir - no need to upload it to artifacts
        trap 'rm -rf $tmpdir/db' EXIT

        export PGDATA="$tmpdir/db"
        export PGHOST="$tmpdir/socket"
        export PGUSER
        export PGDATABASE
        export PGRST_DB_URI="postgresql:///$PGDATABASE?host=$PGHOST&user=$PGUSER"
        export PGRST_DB_SCHEMAS
        export PGRST_DB_ANON_ROLE

        log "Initializing database cluster..."
        # We try to make the database cluster as independent as possible from the host
        # by specifying the timezone, locale and encoding.
        PGTZ=UTC initdb --no-locale --encoding=UTF8 --nosync -U "$PGUSER" --auth=trust \
          >> "$setuplog"

        log "Starting the database cluster..."
        # Instead of listening on a local port, we will listen on a unix domain socket.
        pg_ctl -l "$tmpdir/db.log" start -o "-F -c listen_addresses=\"\" -k $PGHOST" \
          >> "$setuplog"

        log "Waiting for the database cluster to be ready..."
        # Waiting is required for older versions of Postgres (< 10).
        until pg_isready >> "$setuplog"; do
          sleep 0.1
        done

        stop () {
          log "Stopping the database cluster..."
          pg_ctl stop -m i >> "$setuplog"
          rm -rf "$tmpdir/db"
        }
        trap stop EXIT

        log "Loading fixtures..."
        psql -v ON_ERROR_STOP=1 -f "$_arg_fixtures" >> "$setuplog"

        log "Done. Running command..."
        ("$_arg_command" "''${_arg_leftovers[@]}")
      '';

  # Helper script for running a command against all PostgreSQL versions.
  withAll =
    let
      runners =
        builtins.map
          (pg:
            ''
              cat << EOF

              Running against ${pg.name}...

              EOF

              trap 'echo "Failed on ${pg.name}"' exit

              (${withTmpDb pg} "$_arg_command" "''${_arg_leftovers[@]}")

              trap "" exit

              cat << EOF

              Done running against ${pg.name}.

              EOF
            '')
          postgresqlVersions;
    in
    checkedShellScript
      {
        name = "postgrest-with-all";
        docs = "Run command against all supported PostgreSQL versions.";
        args =
          [
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
          ];
        addCommandCompletion = true;
        inRootDir = true;
      }
      (lib.concatStringsSep "\n\n" runners);

  # Create a `postgrest-with-postgresql-` for each PostgreSQL version
  withVersions = builtins.map withTmpDb postgresqlVersions;

in
buildToolbox
{
  name = "postgrest-with";
  tools = [ withAll ] ++ withVersions;
  extra = {
    # make withTools.latest available for other nix files
    latest = withTmpDb (builtins.head postgresqlVersions);
  };
}
