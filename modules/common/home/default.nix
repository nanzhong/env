{ lib, config, pkgs, ... }:
with lib;
let
  keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIvUFgn/Sa929gXPYRlQcnmJtFG/1ntas0q1ShlzmKPt nan@iphone"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRVyHqcSWD8nhiniAfDlV3UIua0/mkINp1XbmcwGHVc nan@ipad"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDH6q/qDHWgll9yMvxdbiBGKL/o6vp6ZfV17ckOKXozK nan@dev"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIh99gRWj1Lot63fO+XB4z+YRttqFDh4SHnTD80XTope nan@wrk"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJUk2KrM+qnxbNxxeux+liBV9EbAlnNodzDwb8v8GbE+ me@nanzho.ng"
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

      keys = mkOption {
        type = types.listOf types.str;
        default = keys;
      };
    };
  };

  config = {
    home-manager.users."${cfg.user}" = {
      home = {
        stateVersion = "22.05";
        file = {
          ".config" = {
            source = ../../../dotfiles/.config;
            recursive = true;
          };

          ".gitconfig" = {
            source = (../../../dotfiles/.gitconfig. + "${cfg.user}");
          };

          ".gitignore" = {
            source = ../../../dotfiles/.gitignore;
          };

          "bin/do-vpn.sh" = mkIf cfg.includeDOVPN {
            source = ../../../bin/do-vpn.sh;
          };
          "bin/hipreport.sh" = mkIf cfg.includeDOVPN {
            source = ../../../bin/hipreport.sh;
          };
          "bin/ical2diary.fish" = {
            source = ../../../bin/ical2diary.fish;
          };
        };
      };
    };
  };
}