{ self, lib, pkgs, ... }:
with lib;
{
  nanzhong = {
    dev.enable = true;
    home =  {
      user = "nan";
      group = "staff";
      includeDirenv = true;
      gitconfig = ".gitconfig.nan.poolside";
    };
  };

  nixpkgs = {
    overlays = [
      (import ../../overlays/fzf-fish.nix)
    ];
  };

  environment.systemPackages = with pkgs; [
    awscli2
  ];

  environment.shells = [ pkgs.fish ];
  services.tailscale.enable = true;
  system.stateVersion = 5;
}
