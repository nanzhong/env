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

  users.users = {
    nan = {
      home = "/mnt/media";
      description = "Nan Zhong";
      group = "plex";
      openssh.authorizedKeys.keys = keys.sshPub;
    };
    josh = {
      home = "/mnt/media";
      description = "Josh Chan";
      group = "plex";
      openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDWOWKghDwtlYzixlQNcLzl6DZaJOMQws+8sUzsEcSSnThd/r/Aes8tIl5kHgdGTVAZaNWQVy3tAvkaX57TRIqtWkU5eWR5Ygl/vslKvUdmrYkIeOzorMs2XBr0kk+e1M4RlNSwYLs022jDJmTpjCad7Lt4FGpIw6TYRcrIcuS8U5cS7r7GWpwdZwExKgxLCGJTwQYNztucvHVBfDfZOjmo+LdaCVXYxRDp9M3toGc3bwKcmuaHHuioiViiUloNB3JU+XAGfke3w9VM2RP5Q8p5+5MOYt+i8Bs1C+UeP5kFACeNdsIomR3/Y69neadFb8Ht2BmqjMJRY6eeDM+3hlQp josh@Chan-PC" ];
    };
  };
}
