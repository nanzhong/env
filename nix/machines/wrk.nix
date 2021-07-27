{ config, pkgs, ... }:
let
  keys = import ../keys.nix;
in {
  imports = [
    ./base.nix
    ./common.nix
  ];

  environment.systemPackages = with pkgs; [
    fly
    git-crypt
    hugo
    mercurial
    mongodb-tools
    openconnect
    vault
  ];

  networking.hostName = "wrk";
  networking.firewall.allowedUDPPorts = [ 137 138 ];
  networking.firewall.allowedTCPPorts = [ 139 445 3000 8080 ];
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

  virtualisation.docker.extraOptions = "--registry-mirror=https://dockerhub-mirror.internal.digitalocean.com";

  services.syncthing = {
    user = "nzhong";
    group = "users";
    dataDir = "/home/nzhong/.syncthing";
    configDir = "/home/nzhong/.syncthing/config";
    declarative = {
      folders = {
        "/home/nzhong/org" = {
          id = "org";
          devices = [ "dev" ];
        };
      };
      devices = {
        dev = {
          id = "X73IBJW-6SZ6EMC-XMXXRFA-RIUBY7L-HF3WNCZ-N2UYZKL-7V3TALA-ZFEN6QE";
          name = "dev";
        };
      };
    };
  };

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
