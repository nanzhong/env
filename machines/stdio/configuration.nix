{ self, lib, pkgs, ... }:
with lib;
{
  nanzhong = {
    dev.enable = true;
    home =  {
      user = "nan";
      group = "staff";
    };
  };

  nixpkgs = {
    overlays = [
      (import ../../overlays/alacritty.nix)
    ];
  };

  environment.shells = [ pkgs.fish ];
  environment.systemPackages = with pkgs; [
    alacritty
  ];

  services.tailscale.enable = true;
  system.checks.verifyBuildUsers = false;
}
