{ pkgs, ... }: {
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  environment.systemPackages = with pkgs; [
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
    package = pkgs.emacsGit-nox;
  };
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
  };
}
