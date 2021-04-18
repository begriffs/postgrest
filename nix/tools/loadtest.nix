{ buildToolbox
, checkedShellScript
, vegeta
, withTools
}:
let
  runner =
    checkedShellScript
      {
        name = "postgrest-loadtest-runner";
        docs = "Run vegeta. Assume PostgREST to be running.";
        args = [
          "ARG_LEFTOVERS([additional vegeta arguments])"
          "ARG_USE_ENV([PGRST_SERVER_UNIX_SOCKET], [], [Unix socket to connect to running PostgREST instance])"
        ];
      }
      ''
        # ARG_USE_ENV only adds defaults or docs for environment variables
        # We manually implement a required check here
        # See also: https://github.com/matejak/argbash/issues/80
        PGRST_SERVER_UNIX_SOCKET="''${PGRST_SERVER_UNIX_SOCKET:?PGRST_SERVER_UNIX_SOCKET is required}"

        ${vegeta}/bin/vegeta -cpus 1 attack \
                                     -unix-socket "$PGRST_SERVER_UNIX_SOCKET" \
                                     -max-workers 1 \
                                     -workers 1 \
                                     -rate 0 \
                                     -duration 5s \
                                     "''${_arg_leftovers[@]}" \
          | ${vegeta}/bin/vegeta encode
      '';

  loadtest =
    checkedShellScript
      {
        name = "postgrest-loadtest";
        docs = "Run the vegeta loadtests with PostgREST.";
        args = [
          "ARG_OPTIONAL_SINGLE([testdir], [t], [Directory to load tests and fixtures from], [./test/loadtest])"
          "ARG_LEFTOVERS([additional vegeta arguments])"
        ];
        inRootDir = true;
      }
      ''
        export PGRST_DB_CONFIG="false"
        export PGRST_DB_POOL="1"
        export PGRST_DB_TX_END="rollback-allow-override"
        export PGRST_LOG_LEVEL="crit"

        # shellcheck disable=SC2145
        ${withTools.pg} --fixtures "$_arg_testdir"/fixtures.sql \
        ${withTools.pgrst} \
        sh -c "cd \"$_arg_testdir\" && ${runner} -targets targets.http \"''${_arg_leftovers[@]}\"" \
          | ${vegeta}/bin/vegeta report -type=text
      '';

  loadtestAgainst =
    let
      name = "postgrest-loadtest-against";
    in
    checkedShellScript
      {
        inherit name;
        docs =
          ''
            Run the vegeta loadtest twice:
            - once on the <target> branch
            - once in the current worktree
          '';
        args = [
          "ARG_POSITIONAL_SINGLE([target], [Commit-ish reference to compare with])"
          "ARG_LEFTOVERS([additional vegeta arguments])"
        ];
        positionalCompletion =
          ''
            if test "$prev" == "${name}"; then
              __gitcomp_nl "$(__git_refs)"
            fi
          '';
        inRootDir = true;
      }
      ''
        cat << EOF

        Running loadtest on "$_arg_target"...

        EOF

        # Runs the test files from the current working tree
        # to make sure both tests are run with the same files.
        ${withTools.git} "$_arg_target" ${loadtest} --testdir "$PWD/test/loadtest" "''${_arg_leftovers[@]}"

        cat << EOF

        Done running on "$_arg_target".

        EOF

        cat << EOF

        Running loadtest on HEAD...

        EOF

        ${loadtest} --testdir "$PWD/test/loadtest" "''${_arg_leftovers[@]}"

        cat << EOF

        Done running on HEAD.

        EOF
      '';

in
buildToolbox {
  name = "postgrest-loadtest";
  tools = [ loadtest loadtestAgainst ];
}
