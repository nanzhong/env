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
      emacs-git-nox
      syncthing
    ];

    services = {
      syncthing = {
        enable = true;
        openDefaultPorts = true;
      };

      emacs = {
        enable = true;
        package = pkgs.emacs-git-nox;
      };
    };
  };
}
