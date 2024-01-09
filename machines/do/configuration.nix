{ self, lib, pkgs, ... }:
with lib;
{
  nanzhong = {
    dev.enable = true;
    home =  {
      user = "nzhong";
      group = "staff";
      includeDirenv = true;
    };
  };

  nixpkgs = {
    overlays = [
      (import ../../overlays/fzf-fish.nix)
    ];
  };

  environment.shells = [ pkgs.fish ];
  services.tailscale.enable = true;
  system.checks.verifyBuildUsers = false;
}
