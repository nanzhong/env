{ lib, config, pkgs, inputs, ... }:
with lib;
let
  cfg = config.nanzhong.home;
in {
  imports = [
    inputs.home-manager.nixosModules.home-manager
    ./default.nix
  ];

  config = {
    users.users = {
      "${cfg.user}" = {
        isNormalUser = true;
        home = "/home/${cfg.user}";
        description = "Nan Zhong";
        extraGroups = [ "wheel" "docker" ];
        shell = pkgs.fish;
        openssh.authorizedKeys.keys = cfg.keys;
      };

      root = {
        openssh.authorizedKeys.keys = cfg.keys;
      };
    };

    programs.ssh.startAgent = true;

    home-manager.users."${cfg.user}" = {
      home.file.".gitconfig" = {
        source = (../../dotfiles/.gitconfig. + "${cfg.user}");
      };
    };
  };
}
