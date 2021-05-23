{ buildToolbox
, checkedShellScript
, circleci-cli
, docker
}:
let
  execute =
    checkedShellScript
      {
        name = "postgrest-ci-execute";
        docs = "Execute CircleCI job locally.";
        args =
          [
            "ARG_POSITIONAL_SINGLE([job], [Job to execute], [nix-test])"
          ];
        inRootDir = true;
        withPath = [ docker ];
        withTmpDir = true;
      }
      ''
        # We override the circle checkout step, because this causes problems with non-root docker images.
        sed -r 's|^(\s*)- checkout$|\1- run: { name: "checkout", command: "cp -rT /tmp/_circleci_local_build_repo ." }|g' \
          .circleci/config.yml > "$tmpdir/config.yml"
        ${circleci-cli}/bin/circleci-cli local execute \
          --config "$tmpdir/config.yml" \
          --job "$_arg_job" 
      '';

  validate =
    checkedShellScript
      {
        name = "postgrest-ci-validate";
        docs = "Validate CI config files.";
        inRootDir = true;
      }
      ''
        ${circleci-cli}/bin/circleci-cli config validate
        # TODO: Make "travis/bin/travis lint" work.
        # TODO: Add cirrus-cli: https://github.com/cirruslabs/cirrus-cli
      '';

in
buildToolbox
{
  name = "postgrest-ci";
  tools = [ execute validate ];
}

