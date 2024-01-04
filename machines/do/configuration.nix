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
      (import ../../overlays/alacritty.nix)
      (import ../../overlays/fzf-fish.nix)
    ];
  };

  environment.shells = [ pkgs.fish ];
  environment.systemPackages = with pkgs; [
    alacritty
  ];

  services.tailscale.enable = true;
  system.checks.verifyBuildUsers = false;
}
