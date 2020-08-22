{ config, pkgs, ... }:
let
  keys = import ../keys.nix;
in {
  imports = [
    ./common.nix
  ];

  networking.hostName = "dev";
  networking.firewall.allowedTCPPorts = [ 3000 3001 8080 ];

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
