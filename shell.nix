# The additional modules below have large dependencies and are therefore
# disabled by default. You can activate them by passing arguments to nix-shell,
# e.g.:
#
#    nix-shell --arg release true
#
# This will provide you with a shell where the `postgrest-release-*` scripts
# are available.
#
# We highly recommend that use the PostgREST binary cache by installing cachix
# (https://app.cachix.org/) and running `cachix use postgrest`.
{ docker ? false
, memory ? false
, release ? false
}:
let
  postgrest =
    import ./default.nix;

  pkgs =
    postgrest.pkgs;

  lib =
    pkgs.lib;

  toolboxes =
    [
      postgrest.cabalTools
      postgrest.devTools
      postgrest.loadtest
      postgrest.nixpkgsTools
      postgrest.style
      postgrest.tests
      postgrest.withTools
    ]
    ++ lib.optional docker postgrest.docker
    ++ lib.optional memory postgrest.memory
    ++ lib.optional release postgrest.release;

in
lib.overrideDerivation postgrest.env (
  base: {
    buildInputs =
      base.buildInputs ++ [
        pkgs.cabal-install
        pkgs.cabal2nix
        pkgs.git
        pkgs.postgresql
        postgrest.hsie.bin
      ]
      ++ toolboxes;

    shellHook =
      ''
        source ${pkgs.bashCompletion}/etc/profile.d/bash_completion.sh
        source ${pkgs.git}/share/git/contrib/completion/git-completion.bash
        source ${postgrest.hsie.bashCompletion}

      ''
      + builtins.concatStringsSep "\n" (
        builtins.map (bashCompletion: "source ${bashCompletion}") (
          builtins.concatLists (builtins.map (toolbox: toolbox.bashCompletion) toolboxes)
        )
      );
  }
)
