{ config, pkgs, ... }:
let
  keys = import ../keys.nix;
in {
  imports = [
    ./base.nix
  ];

  networking.hostName = "media";

  environment.systemPackages = with pkgs; [
    plex
  ];

  services.tailscale.enable = true;
  services.plex = {
    enable = true;
    openFirewall = true;
  };
  services.openssh.extraConfig = ''
    Match Group plex
      ChrootDirectory /mnt/media
      ForceCommand internal-sftp
      AllowTcpForwarding no
  '';

  users.users = {
    nan = {
      home = "/mnt/media";
      description = "Nan Zhong";
      group = "plex";
      openssh.authorizedKeys.keys = keys.sshPub;
    };
  };
}
