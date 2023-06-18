{ self, pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
  ];

  system.stateVersion = "22.05";

  nanzhong = {
    dev.enable = true;
    home =  {
      user = "nzhong";
      includeDOVPN = true;
    };
  };

  environment.systemPackages = with pkgs; [
    fly
    git-crypt
    hugo
    mercurial
    mongodb-tools
    openconnect
    vault
  ];

  virtualisation.docker.extraOptions = "--registry-mirror=https://dockerhub-mirror.internal.digitalocean.com";

  services.syncthing = {
    user = "nzhong";
    group = "users";
    dataDir = "/home/nzhong/.syncthing";
    configDir = "/home/nzhong/.syncthing/config";
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
}
