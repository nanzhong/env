{ lib, config, pkgs, ...}:
with lib;
in
{
  imports = [ ../../common/dev/default.nix ];
  config = {
    environment.systemPackages = with pkgs; [
      syncthing
    ];

    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
    };
  };
}
