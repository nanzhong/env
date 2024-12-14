{ self, lib, pkgs, ... }:
with lib;
{
  nanzhong = {
    dev.enable = true;
    home =  {
      user = "nan";
      group = "staff";
      includeDirenv = true;
      gitconfig = ".gitconfig.nan";
    };
  };

  nixpkgs = {
    overlays = [
      (import ../../overlays/fzf-fish.nix)
    ];
  };

  environment.shells = [ pkgs.nushell ];
  system.checks.verifyBuildUsers = false;
  system.stateVersion = 5;
}
