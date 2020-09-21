{ config, pkgs, ... }:
let
  keys = import ../keys.nix;
in {
  imports = [
    ./common.nix
  ];

  networking.hostName = "dev";
  networking.firewall.allowedUDPPorts = [ 137 138 ];
  networking.firewall.allowedTCPPorts = [ 139 445 3000 3001 8080 ];

  services.samba = {
    enable = true;
    extraConfig = ''
    log level = 2
    ea support = yes
    vfs objects = fruit streams_xattr
    fruit:metadata = stream
    fruit:model = MacSamba
    fruit:posix_rename = yes
    fruit:veto_appledouble = no
    fruit:wipe_intentionally_left_blank_rfork = yes
    fruit:delete_empty_adfiles = yes
    '';
    shares = {
      nan = {
        comment = "/home/nan";
        path = "/home/nan";
        "vfs objects" = "fruit streams_xattr";
        "read only" = "no";
        "guest ok" = "no";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "nan";
        "force group" = "users";
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
