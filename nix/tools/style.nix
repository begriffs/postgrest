{ black
, buildToolbox
, checkedShellScript
, circleci-cli
, git
, hlint
, nixpkgs-fmt
, shellcheck
, silver-searcher
, stylish-haskell
}:
let
  style =
    checkedShellScript
      {
        name = "postgrest-style";
        docs = "Automatically format Haskell, Nix and Python files.";
        inRootDir = true;
      }
      ''
        # Format Nix files
        ${nixpkgs-fmt}/bin/nixpkgs-fmt . > /dev/null 2> /dev/null

        # Format Haskell files
        # --vimgrep fixes a bug in ag: https://github.com/ggreer/the_silver_searcher/issues/753
        ${silver-searcher}/bin/ag -l --vimgrep -g '\.l?hs$' . \
          | xargs ${stylish-haskell}/bin/stylish-haskell -i

        # Format Python files
        ${black}/bin/black . 2> /dev/null
      '';

  # Script to check whether any uncommited changes result from postgrest-style
  styleCheck =
    checkedShellScript
      {
        name = "postgrest-style-check";
        docs = "Check whether postgrest-style results in any uncommited changes.";
        inRootDir = true;
      }
      ''
        ${style}

        ${git}/bin/git diff-index --exit-code HEAD -- '*.hs' '*.lhs' '*.nix'
      '';

  lint =
    checkedShellScript
      {
        name = "postgrest-lint";
        docs = "Lint all Haskell files and bash scripts.";
        inRootDir = true;
      }
      ''
        # Lint Haskell files
        # --vimgrep fixes a bug in ag: https://github.com/ggreer/the_silver_searcher/issues/753
        ${silver-searcher}/bin/ag -l --vimgrep -g '\.l?hs$' . \
          | xargs ${hlint}/bin/hlint -X QuasiQuotes -X NoPatternSynonyms

        # Lint bash scripts
        ${shellcheck}/bin/shellcheck test/create_test_db test/memory-tests.sh

        # Validate circleci config
        ${circleci-cli}/bin/circleci-cli config validate
      '';

in
buildToolbox
{
  name = "postgrest-style";
  tools = [ style styleCheck lint ];
}
