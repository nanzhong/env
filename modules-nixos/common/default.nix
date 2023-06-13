{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.nanzhong.common;
in {
  imports = [ ../../modules/common/default.nix ];
  config = mkIf cfg.enable {
    boot = mkIf pkgs.stdenv.isLinux {
      tmp.cleanOnBoot = true;
      kernel.sysctl = {
        "fs.inotify.max_user_watches" = "1048576";
        "net.ipv4.ip_forward" = "1";
      };
    };

    networking.firewall = {
      allowPing = true;
    };

    virtualisation = {
      docker.enable = true;
      podman.enable = true;
    };

    security.sudo.wheelNeedsPassword = false;

    users.mutableUsers = false;

    programs.mosh.enable = true;

    services.openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };
    services.cron = {
      enable = true;
    };
  };
}
