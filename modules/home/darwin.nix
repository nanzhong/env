{ lib, config, pkgs, inputs, ... }:
with lib;
let
  cfg = config.nanzhong.home;
in {
  imports = [ 
    inputs.home-manager.darwinModules.home-manager  
    ./default.nix
  ];

  config = {
    users.users = {
      "${cfg.user}" = {
        home = "/Users/${cfg.user}";
        shell = pkgs.fish;
        openssh.authorizedKeys.keys = cfg.keys;
      };
    };

    home-manager.users."${cfg.user}" = {
      home.file.".gitconfig" = {
        source = ../../dotfiles/.gitconfig. + "${cfg.user}" + ".darwin";
      };

      home.file.".terminfo" = {
        source = ../../dotfiles/.terminfo;
      };
    };
  };
}
