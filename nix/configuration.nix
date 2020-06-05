{ config, pkgs, ... }:
let
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    ref = "master";
  };
in {
  imports = [
    /etc/nixos/hardware-configuration.nix
    /etc/nixos/networking.nix # generated at runtime by nixos-infect

    "${home-manager}/nixos"

    ./machines/common.nix
    ./machines/current.nix
  ];
}
