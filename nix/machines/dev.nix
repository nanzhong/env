{ config, pkgs, ... }:
let
  keys = import ../keys.nix;
in {
  imports = [
    ./base.nix
    ./common.nix
  ];

  networking.hostName = "dev";
  networking.firewall.allowedUDPPorts = [ 137 138 ];
  networking.firewall.allowedTCPPorts = [ 139 445 3000 3001 8080 ];

  environment.systemPackages = with pkgs; [
    hugo
    fontforge
    tailscale
  ];

  services.tailscale.enable = true;

  services.syncthing = {
    user = "nan";
    group = "users";
    dataDir = "/home/nan/.syncthing";
    configDir = "/home/nan/.syncthing/config";
    declarative = {
      folders = {
        "/home/nan/org" = {
          id = "org";
          devices = [ "wrk" ];
        };
      };
      devices = {
        wrk = {
          id = "AAXYZU5-H55OQVP-JWGZJDL-EZZ4RUQ-S2F37L4-WTKNTVP-FF5TQZN-GGPOPAB";
          name = "wrk";
        };
      };
    };
  };

  users.users = {
    nan = {
      isNormalUser = true;
      home = "/home/nan";
      description = "Nan Zhong";
      extraGroups = [ "wheel" "docker" ];
      shell = pkgs.fish;
      openssh.authorizedKeys.keys = keys.sshPub;
    };
  };
}
