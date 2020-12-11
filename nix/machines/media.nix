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
    josh = {
      home = "/mnt/media";
      description = "Josh Chan";
      group = "plex";
      openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC6LHDUgULxtE3l87oX7o+7bdUJLrGgD+lczepVbJLiRQhb1OVQ9IXU6TNT+jfBFKRimUc84u00AVkDU8raJ4F7cCUJ4xZ9MFUxQvY8K4AMkp1aVqHu/OkoIkbkSwm4fpMUg9B0ceFkAUKbxvs+GJKZ0WWJEwATPoI6RnVxhnp4ri3cWvh5I4UYKGYzA7Td93BdlyhrJJ12SkHO9tp+q08tennhyD3R5etmAsY38+cJ7vd5rA8pu+/3hVdhMhValEKfB1bDiSO8Im6VvtioLOhFaXCW/0tVEBW+gXOnsLyLzH4p0/JZAN9YTBnruecjAlh8rxkU1quKrU5S4rtilUep josh@Chan-PC" ];
    };
  };
}
