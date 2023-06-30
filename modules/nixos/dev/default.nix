{ lib, config, pkgs, ...}:
with lib;
{
  imports = [ ../../common/dev/default.nix ];
  config = {
    environment.systemPackages = with pkgs; [
      emacs-git-nox
      syncthing
    ];

    service = {
      syncthing = {
        enable = true;
        openDefaultPorts = true;
      };

      emacs = {
        enable = true;
        package = emacs-git-nox;
      };
    };
  };
}
