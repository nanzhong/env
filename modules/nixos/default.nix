{ lib, config, pkgs, ... }:
with lib;
{
  imports = [ ../common/default.nix ];
  config = {
    boot = {
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