{ pkgs, ... }:
let
  common = import ./common.nix;
  user = "nan";
in {
  imports = [
    ( common user )
  ];
  home-manager.users."${user}" = {
    home.packages = with pkgs; [
      fontforge
    ];
  };
}
