self: super: {
  fishPlugins = super.fishPlugins.overrideScope (fpself: fpsuper: {
    # TODO fzf-fish makes use of buildFishPlugin's integration of fish tape test
    # runner. However, this does not seem to work as expected, resulting in
    # fishtape not being able to be found in the PATH.
    #
    # Removing /etc/fish and rebuilding fixes this. I suspect it has to do with
    # some behaviour related to fish on nix-darwin, but I haven't had time to
    # debug.
    #
    # Disabling tests for now side-steps this issue.
    fzf-fish = fpsuper.fzf-fish.overrideAttrs (old: {
      doCheck = false;
    });
  });
}
