{ buildToolbox
, cabal-install
, checkedShellScript
, postgrest
}:
let
  devCabalOptions =
    "-f dev --test-show-detail=direct";

  build =
    checkedShellScript
      {
        name = "postgrest-build";
        docs = "Build PostgREST using cabal-install.";
        args = [ "ARG_LEFTOVERS([Cabal arguments])" ];
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        exec ${cabal-install}/bin/cabal v2-build ${devCabalOptions} \
          exe:postgrest lib:postgrest test:spec test:spec-querycost \
          "''${_arg_leftovers[@]}"
      '';

  clean =
    checkedShellScript
      {
        name = "postgrest-clean";
        docs = "Clean the PostgREST project, including all cabal-install artifacts.";
        inRootDir = true;
      }
      ''
        # clean old coverage data, too
        rm -rf .hpc coverage
        exec ${cabal-install}/bin/cabal v2-clean
      '';

  exec =
    checkedShellScript
      {
        name = "postgrest-exec";
        docs = "Execute <command> after building PostgREST with cabal-install.";
        args =
          [
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
          ];
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        ${build} --verbose=0
        exec ${cabal-install}/bin/cabal v2-exec ${devCabalOptions} --verbose=0 -- \
          "$_arg_command" "''${_arg_leftovers[@]}"
      '';

  run =
    checkedShellScript
      {
        name = "postgrest-run";
        docs = "Run PostgREST after building it with cabal-install.";
        args = [ "ARG_LEFTOVERS([PostgREST arguments])" ];
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        exec ${cabal-install}/bin/cabal v2-run ${devCabalOptions} --verbose=0 -- \
          postgrest "''${_arg_leftovers[@]}"
      '';

  test =
    checkedShellScript
      {
        name = "postgrest-test";
        docs = "Run tests using cabal-install.";
        args = [ "ARG_LEFTOVERS([Cabal arguments])" ];
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        exec ${cabal-install}/bin/cabal v2-test ${devCabalOptions} "''${_arg_leftovers[@]}"
      '';

in
buildToolbox
{
  name = "postgrest-cabal";
  tools = [
    build
    clean
    run
  ];
  extra = {
    inherit build exec run test;
  };
}
