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
}
