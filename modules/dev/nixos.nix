{ lib, config, pkgs, ...}:
with lib;
let
  cfg = config.nanzhong.dev;
in {
  imports = [ ./default.nix ];
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      docker
      docker-compose
      syncthing
    ];

    services = {
      syncthing = {
        enable = true;
        openDefaultPorts = true;
      };
    };
  };
}
