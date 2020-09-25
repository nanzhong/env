{ config, pkgs, ... }:
let
  keys = import ../keys.nix;
in {
  imports = [
    ./common.nix
  ];

  networking.hostName = "wrk";
  networking.firewall.allowedUDPPorts = [ 137 138 ];
  networking.firewall.allowedTCPPorts = [ 139 445 3000 ];
  networking.localCommands = ''
    ip=$(ip addr show eth0 | grep -oP '(?<=inet\s)\d+(\.\d+){3}')
    subnet=$(ip route | grep -Po '^\d+(.\d+){3}/\d+(?= dev eth0)')
    gateway=$(ip route | grep -Po '(?<=default via )[.\d]+')
    ip rule delete from $ip table 128 || true
    ip rule add from $ip table 128 || true
    ip route add table 128 to $subnet dev eth0 || true
    ip route add table 128 default via $gateway || true
  '';

  networking.nameservers = [ "10.124.57.141" ];
  networking.search = [ "internal.digitalocean.com" "consul" ];

  # services.samba = {
  #   enable = true;
  #   extraConfig = ''
  #   log level = 2
  #   ea support = yes
  #   vfs objects = fruit streams_xattr
  #   fruit:metadata = stream
  #   fruit:model = MacSamba
  #   fruit:posix_rename = yes
  #   fruit:veto_appledouble = no
  #   fruit:wipe_intentionally_left_blank_rfork = yes
  #   fruit:delete_empty_adfiles = yes
  #   '';
  #   shares = {
  #     nzhong = {
  #       comment = "/home/nzhong";
  #       path = "/home/nzhong";
  #       "vfs objects" = "fruit streams_xattr";
  #       "read only" = "no";
  #       "guest ok" = "no";
  #       "create mask" = "0644";
  #       "directory mask" = "0755";
  #       "force user" = "nzhong";
  #       "force group" = "users";
  #     };
  #   };
  # };

  users.users = {
    nzhong = {
      isNormalUser = true;
      home = "/home/nzhong";
      description = "Nan Zhong";
      extraGroups = [ "wheel" "docker" ];
      shell = pkgs.fish;
      openssh.authorizedKeys.keys = keys.sshPub;
    };
  };
}
