{ buildToolbox
, checkedShellScript
, stack
}:
let
  devStackOptions =
    "--flag 'postgrest:dev'";

  build =
    checkedShellScript
      {
        name = "postgrest-build";
        docs = "Build PostgREST using stack.";
        args = [ "ARG_LEFTOVERS([Stack arguments])" ];
        inRootDir = true;
      }
      ''
        exec ${stack}/bin/stack --nix build ${devStackOptions} --no-run-tests \
          postgrest:exe:postgrest postgrest:lib postgrest:test:spec postgrest:test:spec-querycost \
          "''${_arg_leftovers[@]}"
      '';

  clean =
    checkedShellScript
      {
        name = "postgrest-clean";
        docs = "Clean the PostgREST project, including all stack artifacts.";
        args = [ "ARG_LEFTOVERS([Stack arguments])" ];
        inRootDir = true;
      }
      ''
        # clean old coverage data, too
        rm -rf .hpc coverage
        exec ${stack}/bin/stack clean "''${_arg_leftovers[@]}"
      '';

  exec =
    checkedShellScript
      {
        name = "postgrest-exec";
        docs = "Execute <command> after building PostgREST with stack.";
        args =
          [
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
          ];
        inRootDir = true;
      }
      ''
        ${build} --silent
        exec ${stack}/bin/stack --nix --silent exec -- \
          "$_arg_command" "''${_arg_leftovers[@]}"
      '';

  run =
    checkedShellScript
      {
        name = "postgrest-run";
        docs = "Run PostgREST after building it with stack.";
        args = [ "ARG_LEFTOVERS([PostgREST arguments])" ];
        inRootDir = true;
      }
      ''
        exec ${exec} postgrest "''${_arg_leftovers[@]}"
      '';

  test =
    checkedShellScript
      {
        name = "postgrest-test";
        docs = "Run tests using stack.";
        args = [ "ARG_LEFTOVERS([Stack arguments])" ];
        inRootDir = true;
      }
      ''
        exec ${stack}/bin/stack --nix test ${devStackOptions} "''${_arg_leftovers[@]}"
      '';

in
buildToolbox
{
  name = "postgrest-stack";
  tools = [
    build
    clean
    run
  ];
  extra = {
    inherit build exec run test;
  };
}
