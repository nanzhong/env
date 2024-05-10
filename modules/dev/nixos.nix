{ lib, config, pkgs, ...}:
with lib;
let
  cfg = config.nanzhong.dev;
  emacs-nixos = ((pkgs.emacsPackagesFor pkgs.emacs-git-nox).emacsWithPackages (
    epkgs: [ epkgs.jinx ]
  ));
in {
  imports = [ ./default.nix ];
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      docker
      docker-compose
      emacs-nixos
      syncthing
    ];

    services = {
      syncthing = {
        enable = true;
        openDefaultPorts = true;
      };

      emacs = {
        enable = true;
        package = emacs-nixos;
      };
    };
  };
}
