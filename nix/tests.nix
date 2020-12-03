# Utilities for running the PostgREST test suite

{ buildEnv
, cabal-install
, checkedShellScript
, curl
, devCabalOptions
, ghc
, gnugrep
, haskell
, hpc-codecov
, lib
, postgresql
, postgresqlVersions
, postgrest
, postgrestProfiled
, python3
, runtimeShell
, yq
}:
let
  # Wrap the `test/with_tmp_db` script with the required dependencies from Nix.
  withTmpDb =
    postgresql:
    checkedShellScript
      {
        name = "postgrest-test-withtmpdb-${postgresql.name}";
        docs = "Run the given command in a temporary database";
        inRootDir = true;
      }
      ''
        export PATH=${postgresql}/bin:"$PATH"

        exec test/with_tmp_db "$@"
      '';

  # Script to run the Haskell test suite against a specific version of
  # PostgreSQL.
  testSpec =
    name: postgresql:
    checkedShellScript
      {
        inherit name;
        docs = "Run the Haskell test suite against ${postgresql.name}.";
        inRootDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        cat << EOF

        Running spec against ${postgresql.name}...

        EOF

        trap 'echo "Failed on ${postgresql.name}"' exit

        ${withTmpDb postgresql} ${cabal-install}/bin/cabal v2-test ${devCabalOptions}

        trap "" exit

        cat << EOF

        Done running spec against ${postgresql.name}.

        EOF
      '';

  testSpecIdempotence =
    name: postgrestql:
    checkedShellScript
      {
        inherit name;
        docs = "Check that the Haskell tests can be run multiple times against the same db.";
        inRootDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        ${withTmpDb postgresql} ${runtimeShell} -c " \
          ${cabal-install}/bin/cabal v2-test ${devCabalOptions} && \
          ${cabal-install}/bin/cabal v2-test ${devCabalOptions}"
      '';

  # Create a `testSpec` for each PostgreSQL version that we want to test
  # against.
  testSpecVersions =
    builtins.map
      ({ name, postgresql }:
        (testSpec "postgrest-test-spec-${name}" postgresql).bin)
      postgresqlVersions;

  # Helper script for running the tests against all PostgreSQL versions.
  testSpecAllVersions =
    let
      testRunners =
        map (test: "${test}/bin/${test.name}") testSpecVersions;
    in
    checkedShellScript
      {
        name = "postgrest-test-spec-all";
        docs = "Run the Haskell tests against all supported PostgreSQL versions.";
        inRootDir = true;
      }
      (lib.concatStringsSep "\n" testRunners);

  ioTestPython =
    python3.withPackages (ps: [
      ps.pyjwt
      ps.pytest
      ps.pytest_xdist
      ps.pyyaml
      ps.requests
      ps.requests-unixsocket
    ]);

  testIO =
    name: postgresql:
    checkedShellScript
      {
        inherit name;
        docs = "Run the pytest-based IO tests.";
        inRootDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        ${cabal-install}/bin/cabal v2-build ${devCabalOptions}
        ${cabal-install}/bin/cabal v2-exec ${withTmpDb postgresql} \
          ${ioTestPython}/bin/pytest -- -v test/io-tests "$@"
      '';

  testMemory =
    name: postgresql:
    checkedShellScript
      {
        inherit name;
        docs = "Run the memory tests.";
        inRootDir = true;
      }
      ''
        export PATH="${postgrestProfiled}/bin:${curl}/bin:$PATH"

        ${withTmpDb postgresql} test/memory-tests.sh
      '';

  dumpSchema =
    name: postgresql:
    checkedShellScript
      {
        inherit name;
        docs = "Dump the loaded schema's DbStructure as a yaml file.";
        inRootDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        ${withTmpDb postgresql} \
            ${cabal-install}/bin/cabal v2-run ${devCabalOptions} --verbose=0 -- \
            postgrest --dump-schema \
            | ${yq}/bin/yq -y .
      '';

  coverage =
    name: postgresql:
    checkedShellScript
      {
        inherit name;
        docs = "Run spec and io tests while collecting hpc coverage data.";
        inRootDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        # clean up previous coverage reports
        mkdir -p coverage
        rm -rf coverage/*

        # temporary directory to collect data in
        tmpdir="$(mktemp -d)"

        # we keep the tmpdir when an error occurs for debugging and only remove it on success
        trap 'echo Temporary directory kept at: $tmpdir' ERR SIGINT SIGTERM

        # build once before running all the tests
        ${cabal-install}/bin/cabal v2-build ${devCabalOptions} --enable-tests all

        # collect all tests
        HPCTIXFILE="$tmpdir"/io.tix \
        ${withTmpDb postgresql} ${cabal-install}/bin/cabal v2-exec ${devCabalOptions} \
          ${ioTestPython}/bin/pytest -- -v test/io-tests
          
        HPCTIXFILE="$tmpdir"/spec.tix \
        ${withTmpDb postgresql} ${cabal-install}/bin/cabal v2-test ${devCabalOptions}

        # collect all the tix files
        ${ghc}/bin/hpc sum  --union --exclude=Paths_postgrest --output="$tmpdir"/tests.tix "$tmpdir"/io.tix "$tmpdir"/spec.tix

        # prepare the overlay
        ${ghc}/bin/hpc overlay --output="$tmpdir"/overlay.tix test/coverage.overlay
        ${ghc}/bin/hpc sum --union --output="$tmpdir"/tests-overlay.tix "$tmpdir"/tests.tix "$tmpdir"/overlay.tix

        # check nothing in the overlay is actually tested
        ${ghc}/bin/hpc map --function=inv --output="$tmpdir"/inverted.tix "$tmpdir"/tests.tix
        ${ghc}/bin/hpc combine --function=sub \
          --output="$tmpdir"/check.tix "$tmpdir"/overlay.tix "$tmpdir"/inverted.tix
        # returns zero exit code if any count="<non-zero>" lines are found, i.e.
        # something is covered by both the overlay and the tests
        if ${ghc}/bin/hpc report --xml "$tmpdir"/check.tix | ${gnugrep}/bin/grep -qP 'count="[^0]'
        then
          ${ghc}/bin/hpc markup --highlight-covered --destdir=coverage/overlay "$tmpdir"/overlay.tix || true
          ${ghc}/bin/hpc markup --highlight-covered --destdir=coverage/check "$tmpdir"/check.tix || true
          echo "ERROR: Something is covered by both the tests and the overlay:"
          echo "file://$(pwd)/coverage/check/hpc_index.html"
          exit 1
        else
          # copy the result .tix file to the coverage/ dir to make it available to postgrest-coverage-draft-overlay, too
          cp "$tmpdir"/tests-overlay.tix coverage/postgrest.tix
          # prepare codecov json report
          ${hpc-codecov}/bin/hpc-codecov --mix=.hpc --out=coverage/codecov.json coverage/postgrest.tix

          # create html and stdout reports
          # TODO: The markup command fails when run outside nix-shell (i.e. in CI!)
          # Need to fix it properly in the future instead of adding the || true
          ${ghc}/bin/hpc markup --destdir=coverage coverage/postgrest.tix || true
          echo "file://$(pwd)/coverage/hpc_index.html"
          ${ghc}/bin/hpc report coverage/postgrest.tix "$@"
        fi

        rm -rf "$tmpdir"
      '';

  coverageDraftOverlay =
    name:
    checkedShellScript
      {
        inherit name;
        docs = "Create a draft overlay from current coverage report.";
        inRootDir = true;
      }
      ''
        ${ghc}/bin/hpc draft --output=test/coverage.overlay coverage/postgrest.tix
        sed -i 's|^module \(.*\):|module \1/|g' test/coverage.overlay
      '';

in
# Create an environment that contains all the utility scripts for running tests
  # that we defined above.
buildEnv
  {
    name =
      "postgrest-tests";

    paths =
      [
        (testSpec "postgrest-test-spec" postgresql).bin
        (testSpecIdempotence "postgrest-test-spec-idempotence" postgresql).bin
        testSpecAllVersions.bin
        (testIO "postgrest-test-io" postgresql).bin
        (dumpSchema "postgrest-dump-schema" postgresql).bin
        (coverage "postgrest-coverage" postgresql).bin
        (coverageDraftOverlay "postgrest-coverage-draft-overlay").bin
      ] ++ testSpecVersions;
  }
  # The memory tests have large dependencies (a profiled build of PostgREST)
  # and are run less often than the spec tests, so we don't include them in
  # the default test environment. We make them available through a separate attribute:
  // {
  memoryTests =
    (testMemory "postgrest-test-memory" postgresql).bin;
}
