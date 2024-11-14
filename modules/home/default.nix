{ lib, config, pkgs, inputs, ... }:
with lib;
let
  keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIvUFgn/Sa929gXPYRlQcnmJtFG/1ntas0q1ShlzmKPt nan@iphone"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRVyHqcSWD8nhiniAfDlV3UIua0/mkINp1XbmcwGHVc nan@ipad"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDH6q/qDHWgll9yMvxdbiBGKL/o6vp6ZfV17ckOKXozK nan@dev"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJUk2KrM+qnxbNxxeux+liBV9EbAlnNodzDwb8v8GbE+ me@nanzho.ng"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIYtYfxEmZmLmNTpNRcR7eF8IpU+wdcTZSHWGA0SNqTw nan@devpi"
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

      includeDirenv = mkOption {
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
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      users."${cfg.user}" = {
        home = {
          stateVersion = "22.05";

          file = {
            ".config" = {
              source = ../../dotfiles/.config;
              recursive = true;
            };
            ".terminfo" = {
              source = ../../dotfiles/.terminfo;
            };
            ".gitignore" = {
              source = ../../dotfiles/.gitignore;
            };
            "bin/do-vpn.sh" = mkIf cfg.includeDOVPN {
              source = ../../bin/do-vpn.sh;
            };
            "bin/hipreport.sh" = mkIf cfg.includeDOVPN {
              source = ../../bin/hipreport.sh;
            };
            "bin/ical2diary.fish" = {
              source = ../../bin/ical2diary.fish;
            };
            "bin/helix-git-blame" = {
              source = ../../bin/helix-git-blame;
            };
            "bin/helix-gh-browse" = {
              source = ../../bin/helix-gh-browse;
            };
          };

          activation.nvimLazyLock = ''
            cp ${../../dotfiles/.config/nvim/lazy-lock.json.snapshot} ~/.config/nvim/lazy-lock.json
            chmod 0644 ~/.config/nvim/lazy-lock.json
          '';
        };

        programs = {
          direnv = mkIf cfg.includeDirenv {
            enable = true;
            nix-direnv.enable = true;
          };

          fish.enable = true;
        };
      };
    };
  };
}
