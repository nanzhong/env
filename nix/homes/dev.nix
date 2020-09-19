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
      hugo
      fontforge
    ];

    home.file = {
      ".gitconfig" = {
        source = ../../dotfiles/.gitconfig.nan;
      };
    };
  };
}
