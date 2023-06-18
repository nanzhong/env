{ self, pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
  ];

  system.stateVersion = "22.05";

  nanzhong = {
    dev.enable = true;
    home =  {
      user = "nan";
    };
  };

  environment.systemPackages = with pkgs; [
    hugo
    fontforge
    tailscale
  ];

  services.syncthing = {
    user = "nan";
    group = "users";
    dataDir = "/home/nan/.syncthing";
    configDir = "/home/nan/.syncthing/config";
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

  services.tailscale.enable = true;
}
