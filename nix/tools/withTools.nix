{ bashCompletion
, buildToolbox
, cabal-install
, checkedShellScript
, curl
, devCabalOptions
, git
, lib
, postgresqlVersions
, postgrest
, writeText
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
        withTmpDir = true;
      }
      ''
        # avoid starting multiple layers of withTmpDb
        if test -v PGRST_DB_URI; then
          exec "$@"
        fi
        
        export PATH=${postgresql}/bin:"$PATH"
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

  withGit =
    checkedShellScript
      {
        name = "postgrest-with-git";
        docs =
          ''
            Create a new worktree of the postgrest repo in a temporary directory and
            check out <commit>, then run <command> with arguments inside the temporary folder.
          '';
        args =
          [
            "ARG_POSITIONAL_SINGLE([commit], [Commit-ish reference to run command with])"
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
          ];
        addCommandCompletion = true; # TODO: first positional argument needs git commit completion
        inRootDir = true;
      }
      ''
        # not using withTmpDir here, because we don't want to keep the directory on error
        tmpdir="$(mktemp -d)"
        trap 'rm -rf "$tmpdir"' EXIT

        ${git}/bin/git worktree add -f "$tmpdir" "$_arg_commit" > /dev/null

        cd "$tmpdir"
        ("$_arg_command" "''${_arg_leftovers[@]}")

        ${git}/bin/git worktree remove -f "$tmpdir" > /dev/null
      '';

  legacyConfig =
    writeText "legacy.conf"
      ''
        # Using this config file to support older postgrest versions for `postgrest-loadtest-against`
        db-uri="$(PGRST_DB_URI)"
        db-schema="$(PGRST_DB_SCHEMAS)"
        db-anon-role="$(PGRST_DB_ANON_ROLE)"
        db-pool="$(PGRST_DB_POOL)"
        server-unix-socket="$(PGRST_SERVER_UNIX_SOCKET)"
        log-level="$(PGRST_LOG_LEVEL)"
      '';

  waitForPgrst =
    checkedShellScript
      {
        name = "postgrest-wait-for-pgrst";
        docs = "Wait for PostgREST to be up and running.";
        args = [
          "ARG_USE_ENV([PGRST_SERVER_UNIX_SOCKET], [], [Unix socket to check for running PostgREST instance])"
        ];
      }
      ''
        # ARG_USE_ENV only adds defaults or docs for environment variables
        # We manually implement a required check here
        # See also: https://github.com/matejak/argbash/issues/80
        PGRST_SERVER_UNIX_SOCKET="''${PGRST_SERVER_UNIX_SOCKET:?PGRST_SERVER_UNIX_SOCKET is required}"

        function check_status () {
          ${curl}/bin/curl -s -o /dev/null -w "%{http_code}" --unix-socket "$PGRST_SERVER_UNIX_SOCKET" http://localhost/
        }

        while [[ "$(check_status)" != "200" ]];
           do sleep 0.1;
        done
      '';

  withPgrst =
    checkedShellScript
      {
        name = "postgrest-with-pgrst";
        docs = "Build and run PostgREST and run <command> with PGRST_SERVER_UNIX_SOCKET set.";
        args =
          [
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
          ];
        addCommandCompletion = true;
        inRootDir = true;
        withEnv = postgrest.env;
        withTmpDir = true;
      }
      ''
        export PGRST_SERVER_UNIX_SOCKET="$tmpdir"/postgrest.socket

        ${cabal-install}/bin/cabal v2-build ${devCabalOptions} > "$tmpdir"/build.log
        ${cabal-install}/bin/cabal v2-run ${devCabalOptions} --verbose=0 -- \
          postgrest ${legacyConfig} > "$tmpdir"/run.log &
        
        pid=$!
        cleanup() {
          kill $pid || true
        }
        trap cleanup exit

        # wait for PostgREST to be ready
        timeout -s TERM 5 ${waitForPgrst}

        ("$_arg_command" "''${_arg_leftovers[@]}")
      '';

in
buildToolbox
{
  name = "postgrest-with";
  tools = [ withAll withGit withPgrst ] ++ withVersions;
  extra = {
    # make withTools available for other nix files
    pg = withTmpDb (builtins.head postgresqlVersions);
    git = withGit;
    pgrst = withPgrst;
  };
}
