{ lib, config, pkgs, ... }:
with lib;
{
  imports = [ ../../modules/common/default.nix ];
  config = {
    homebrew.enable = true;

    # This fixes the inpcorrect path ordering
    programs.fish.loginShellInit = ''for p in (string split " " $NIX_PROFILES); fish_add_path --prepend --move $p/bin; end'';
  };
}
