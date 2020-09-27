{ pkgs, ... }:
let
  common = import ./common.nix;
  user = "nzhong";
in {
  imports = [
    ( common user )
  ];
  home-manager.users."${user}" = {
    home.file = {
      ".gitconfig" = {
        source = ../../dotfiles/.gitconfig.nzhong;
      };

      "bin/do-vpn.sh" = {
        source = ../../bin/do-vpn.sh;
      };
      "bin/hipreport.sh" = {
        source = ../../bin/hipreport.sh;
      };
    };
  };
}
