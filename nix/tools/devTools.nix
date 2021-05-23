{ buildToolbox
, buildTools
, cachix
, cabalTools
, checkedShellScript
, entr
, graphviz
, hsie
, nix
, silver-searcher
, style
, tests
}:
let
  watch =
    checkedShellScript
      {
        name = "postgrest-watch";
        docs =
          ''
            Watch the project for changes and reinvoke the given command.

            Example:
              postgrest-watch postgrest-test-io
          '';
        args =
          [
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
          ];
        addCommandCompletion = true;
        redirectTixFiles = false; # will be done by sub-command
        inRootDir = true;
      }
      ''
        while true; do
          (! ${silver-searcher}/bin/ag -l . | ${entr}/bin/entr -dr "$_arg_command" "''${_arg_leftovers[@]}")
        done
      '';

  pushCachix =
    checkedShellScript
      {
        name = "postgrest-push-cachix";
        docs = ''
          Push all build artifacts to cachix.

          Requires authentication with `cachix authtoken ...`.
        '';
        inRootDir = true;
      }
      ''
        ${nix}/bin/nix-instantiate \
          | while read -r drv; do
              ${nix}/bin/nix-store -qR --include-outputs "$drv"
            done \
          | ${cachix}/bin/cachix push postgrest
      '';

  check =
    checkedShellScript
      {
        name = "postgrest-check";
        docs =
          ''
            Run most checks that will also run on CI.

            This currently excludes the memory tests, as those are particularly
            expensive.
          '';
        inRootDir = true;
      }
      ''
        ${tests}/bin/postgrest-with-all ${tests}/bin/postgrest-test-spec
        ${tests}/bin/postgrest-test-spec-idempotence
        ${tests}/bin/postgrest-test-io
        ${style}/bin/postgrest-lint
        ${style}/bin/postgrest-style-check
      '';

  dumpMinimalImports =
    checkedShellScript
      {
        name = "postgrest-dump-minimal-imports";
        docs = "Dump minimal imports into given directory.";
        args = [ "ARG_POSITIONAL_SINGLE([dumpdir], [Output directory])" ];
        inRootDir = true;
        withTmpDir = true;
      }
      ''
        mkdir -p "$_arg_dumpdir"
        # Running cabalTools instead of buildTools on purpose,
        # because the cabal arguments don't work with stack
        ${cabalTools.build} \
          --builddir="$tmpdir" \
          --ghc-option=-ddump-minimal-imports \
          --ghc-option=-dumpdir="$_arg_dumpdir" \
          1>&2

        # Fix OverloadedRecordFields imports
        # shellcheck disable=SC2016
        sed -E 's/\$sel:.*://g' -i "$_arg_dumpdir"/*
      '';

  hsieMinimalImports =
    checkedShellScript
      {
        name = "postgrest-hsie-minimal-imports";
        docs = "Run hsie with a provided dump of minimal imports.";
        args = [ "ARG_LEFTOVERS([hsie arguments])" ];
        withTmpDir = true;
      }
      ''
        ${dumpMinimalImports} "$tmpdir"
        ${hsie} "$tmpdir" "''${_arg_leftovers[@]}"
      '';

  hsieGraphModules =
    checkedShellScript
      {
        name = "postgrest-hsie-graph-modules";
        docs = "Create a PNG graph of modules imported within the codebase.";
        args = [ "ARG_POSITIONAL_SINGLE([outfile], [Output filename])" ];
      }
      ''
        ${hsie} graph-modules main src | ${graphviz}/bin/dot -Tpng -o "$_arg_outfile"
      '';

  hsieGraphSymbols =
    checkedShellScript
      {
        name = "postgrest-hsie-graph-symbols";
        docs = "Create a PNG graph of symbols imported within the codebase.";
        args = [ "ARG_POSITIONAL_SINGLE([outfile], [Output filename])" ];
      }
      ''
        ${hsieMinimalImports} graph-symbols | ${graphviz}/bin/dot -Tpng -o "$_arg_outfile"
      '';

in
buildToolbox
{
  name = "postgrest-dev";
  tools = [
    watch
    pushCachix
    check
    dumpMinimalImports
    hsieMinimalImports
    hsieGraphModules
    hsieGraphSymbols
  ];
}
