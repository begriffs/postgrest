# The memory tests have large dependencies (a profiled build of PostgREST)
# and are run less often than the spec tests, so we don't include them in
# the default test environment. We make them available through a separate module.
{ buildToolbox
, checkedShellScript
, curl
, postgrestProfiled
, withTools
}:
let
  test =
    checkedShellScript
      {
        name = "postgrest-test-memory";
        docs = "Run the memory tests.";
        inRootDir = true;
        withPath = [ postgrestProfiled curl ];
      }
      ''
        ${withTools.latest} test/memory-tests.sh
      '';

in
buildToolbox
{
  name = "postgrest-memory";
  tools = [ test ];
}
