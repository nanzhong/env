{ lib, config, pkgs, ... }:
with lib;
let
  keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRVyHqcSWD8nhiniAfDlV3UIua0/mkINp1XbmcwGHVc nan@ipad"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDH6q/qDHWgll9yMvxdbiBGKL/o6vp6ZfV17ckOKXozK nan@dev"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIh99gRWj1Lot63fO+XB4z+YRttqFDh4SHnTD80XTope nan@wrk"
  ];
  cfg = config.nanzhong.home;
in {
  options = {
    nanzhong.home = {
      user = mkOption {
        type = types.str;
        default = "nan";
      };

      group = mkOption {
        type = types.str;
        default = "users";
      };

      includeDOVPN = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = {
    users.users = {
      "${cfg.user}" = {
        isNormalUser = true;
        home = "/home/${cfg.user}";
        description = "Nan Zhong";
        extraGroups = [ "wheel" "docker" ];
        shell = pkgs.fish;
        openssh.authorizedKeys.keys = keys;
      };

      root = {
        openssh.authorizedKeys.keys = keys;
      };
    };

    home-manager.users."${cfg.user}" = {
      home.file = {
        ".config" = {
          source = ../../dotfiles/.config;
          recursive = true;
        };

        ".gitconfig" = {
          source = (../../dotfiles/.gitconfig. + "${cfg.user}");
        };

        ".gitignore" = {
          source = ../../dotfiles/.gitignore;
        };

        ".terminfo" = {
          source = ../../dotfiles/.terminfo;
          recursive = true;
        };

        "bin/do-vpn.sh" = mkIf cfg.includeDOVPN {
          source = ../../scripts/do-vpn.sh;
        };
        "bin/hipreport.sh" = mkIf cfg.includeDOVPN {
          source = ../../scripts/hipreport.sh;
        };
      };
    };
  };
}