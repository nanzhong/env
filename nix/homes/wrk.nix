{ pkgs, ... }:
let
  common = import ./common.nix;
  user = "nzhong";
in {
  imports = [
    ( common user )
  ];
  home-manager.users."${user}" = {
    home.packages = with pkgs; [
      breezy
      fly
      git-crypt
      mercurial
      openconnect
    ];

    home.file = {
      ".gitconfig" = {
        source = ../../dotfiles/.gitconfig.nzhong;
      };
    };
  };
}
