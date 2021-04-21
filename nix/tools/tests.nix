{ buildToolbox
, cabal-install
, checkedShellScript
, devCabalOptions
, ghc
, git
, glibcLocales
, gnugrep
, haskell
, hpc-codecov
, postgrest
, python3
, runtimeShell
, withTools
, yq
}:
let
  testSpec =
    checkedShellScript
      {
        name = "postgrest-test-spec";
        docs = "Run the Haskell test suite";
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        ${withTools.latest} ${cabal-install}/bin/cabal v2-test ${devCabalOptions}
      '';

  testSpecIdempotence =
    checkedShellScript
      {
        name = "postgrest-test-spec-idempotence";
        docs = "Check that the Haskell tests can be run multiple times against the same db.";
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        ${withTools.latest} ${runtimeShell} -c " \
          ${cabal-install}/bin/cabal v2-test ${devCabalOptions} && \
          ${cabal-install}/bin/cabal v2-test ${devCabalOptions}"
      '';

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
    checkedShellScript
      {
        name = "postgrest-test-io";
        docs = "Run the pytest-based IO tests.";
        args = [ "ARG_LEFTOVERS([pytest arguments])" ];
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        ${cabal-install}/bin/cabal v2-build ${devCabalOptions}
        ${cabal-install}/bin/cabal v2-exec ${withTools.latest} \
          ${ioTestPython}/bin/pytest -- -v test/io-tests "''${_arg_leftovers[@]}"
      '';

  dumpSchema =
    checkedShellScript
      {
        name = "postgrest-dump-schema";
        docs = "Dump the loaded schema's DbStructure as a yaml file.";
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        ${withTools.latest} \
            ${cabal-install}/bin/cabal v2-run ${devCabalOptions} --verbose=0 -- \
            postgrest --dump-schema \
            | ${yq}/bin/yq -y .
      '';

  coverage =
    checkedShellScript
      {
        name = "postgrest-coverage";
        docs = "Run spec and io tests while collecting hpc coverage data.";
        args = [ "ARG_LEFTOVERS([hpc report arguments])" ];
        inRootDir = true;
        redirectTixFiles = false;
        withEnv = postgrest.env;
        withTmpDir = true;
      }
      ''
        export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"

        # clean up previous coverage reports
        mkdir -p coverage
        rm -rf coverage/*

        # build once before running all the tests
        ${cabal-install}/bin/cabal v2-build ${devCabalOptions} --enable-tests all

        # collect all tests
        HPCTIXFILE="$tmpdir"/io.tix \
        ${withTools.latest} ${cabal-install}/bin/cabal v2-exec ${devCabalOptions} \
          ${ioTestPython}/bin/pytest -- -v test/io-tests
          
        HPCTIXFILE="$tmpdir"/spec.tix \
        ${withTools.latest} ${cabal-install}/bin/cabal v2-test ${devCabalOptions}

        # collect all the tix files
        ${ghc}/bin/hpc sum  --union --exclude=Paths_postgrest --output="$tmpdir"/tests.tix "$tmpdir"/io*.tix "$tmpdir"/spec.tix

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
          ${ghc}/bin/hpc markup --destdir=coverage coverage/postgrest.tix
          echo "file://$(pwd)/coverage/hpc_index.html"
          ${ghc}/bin/hpc report coverage/postgrest.tix "''${_arg_leftovers[@]}"
        fi
      '';

  coverageDraftOverlay =
    checkedShellScript
      {
        name = "postgrest-coverage-draft-overlay";
        docs = "Create a draft overlay from current coverage report.";
        inRootDir = true;
      }
      ''
        ${ghc}/bin/hpc draft --output=test/coverage.overlay coverage/postgrest.tix
        sed -i 's|^module \(.*\):|module \1/|g' test/coverage.overlay
      '';

  dumpSchemaUpdatePython =
    python3.withPackages (ps: [ ps.pyyaml ]);

  dumpSchemaUpdate =
    checkedShellScript
      {
        name = "postgrest-dump-schema-update";
        docs = "Update the dumped schema.";
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        ${withTools.latest} \
            ${cabal-install}/bin/cabal v2-exec ${devCabalOptions} --verbose=0 -- \
            ${dumpSchemaUpdatePython}/bin/python test/dump-schema/update.py
      '';

  dumpSchemaCheck =
    checkedShellScript
      {
        name = "postgrest-dump-schema-check";
        docs = "Check whether re-dumping the schema results in any changes.";
        inRootDir = true;
      }
      ''
        ${dumpSchemaUpdate}
        ${git}/bin/git add test/dump-schema/expected
        ${git}/bin/git diff-index --cached -p --exit-code HEAD
      '';

in
buildToolbox
{
  name = "postgrest-tests";
  tools =
    [
      testSpec
      testSpecIdempotence
      testIO
      dumpSchema
      coverage
      coverageDraftOverlay
      dumpSchemaUpdate
      dumpSchemaCheck
    ];
}
