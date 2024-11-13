{ self, lib, pkgs, ... }:
with lib;
{
  nanzhong = {
    dev.enable = true;
    home =  {
      user = "nan";
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
  system.stateVersion = 5;
}
