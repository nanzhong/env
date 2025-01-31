{ lib, config, pkgs, inputs, system, ... }:
with lib;
let
  keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIvUFgn/Sa929gXPYRlQcnmJtFG/1ntas0q1ShlzmKPt nan@iphone"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRVyHqcSWD8nhiniAfDlV3UIua0/mkINp1XbmcwGHVc nan@ipad"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDH6q/qDHWgll9yMvxdbiBGKL/o6vp6ZfV17ckOKXozK nan@dev"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJUk2KrM+qnxbNxxeux+liBV9EbAlnNodzDwb8v8GbE+ me@nanzho.ng"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIYtYfxEmZmLmNTpNRcR7eF8IpU+wdcTZSHWGA0SNqTw nan@devpi"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGbiU4cHpFhpAvsrSYIA6gI1Xz9564tz0OVhPZlsSFLR nan@poolside.ai"
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

      gitconfig = mkOption {
        type = types.str;
        default = ".gitconfig.${cfg.user}";
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
            ".gitconfig" = {
              source = ../../dotfiles/${cfg.gitconfig};
            };
            ".gitignore" = {
              source = ../../dotfiles/.gitignore;
            };
            ".ssh/config" = {
              source = ../../dotfiles/.ssh/config;
            };
          };

          activation.nvimLazyLock = inputs.home-manager.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            cp ${../../dotfiles/.config/nvim/lazy-lock.json.snapshot} ~/.config/nvim/lazy-lock.json
            chmod 0644 ~/.config/nvim/lazy-lock.json
            echo "Syncing neovim plugins"
            $DRY_RUN_CMD ${pkgs.neovim}/bin/nvim --headless "+Lazy! restore" +qa
          '';
        };

        programs = {
          direnv = mkIf cfg.includeDirenv {
            enable = true;
            nix-direnv.enable = true;
          };

          nushell = {
            enable = true;
            envFile.text = (builtins.replaceStrings [
              "NIX_BASH_ENV_NU_MODULE"
            ] [
              "${inputs.bash-env-nushell.packages.${system}.default}/bash-env.nu"
            ] (builtins.readFile ../../dotfiles/.config/nushell/env.nu));
            configFile.source = ../../dotfiles/.config/nushell/config.nu;
            loginFile.source = ../../dotfiles/.config/nushell/login.nu;
          };
        };
      };
    };
  };
}
