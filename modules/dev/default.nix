{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.nanzhong.dev;
in {
  options.nanzhong.dev = {
    enable = mkEnableOption "Dev configuration";
  };

  config =  mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      asciinema
      bpytop
      cloc
      direnv
      dive
      doctl
      gh
      go
      gopls
      hunspell
      hunspellDicts.en-us
      hunspellDicts.en-us-large
      inotify-tools
      kubectl
      kubernetes-helm
      lua
      mariadb-client
      neovim
      nodejs
      python
      python3
      ruby
      s3cmd
      shellcheck
      sqlite
      starship
      syncthing
      ttfautohint
      z-lua
    ];

    services.emacs = {
      enable = true;
      package = (pkgs.emacsGit-nox.override { nativeComp = true; } );
    };
    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
    };
  };
}
