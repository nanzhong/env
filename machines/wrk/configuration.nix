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
    aerospace
    awscli2
    nushell
  ];

  environment.shells = [ pkgs.nushell ];
  system.stateVersion = 5;
}
