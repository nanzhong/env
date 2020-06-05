{ pkgs, ... }: {
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  home.packages = with pkgs; [
    breezy
    direnv
    docker-compose
    doctl
    fly
    fzf
    git-crypt
    gnumake
    gnupg
    go
    ispell
    kubectl
    kubernetes-helm
    lua
    mariadb-client
    mercurial
    nodejs
    openconnect
    python
    ruby
    starship
    unzip
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit-nox;
  };
  services.emacs.enable = true;

  home.file = {
    ".config" = {
      source = ../dotfiles/.config;
      recursive = true;
    };

    ".tmux.conf" = {
      source = ../dotfiles/.tmux.conf;
    };

    ".gitconfig" = {
      source = ../dotfiles/.gitconfig;
    };

    ".gitignore" = {
      source = ../dotfiles/.gitignore;
    };

    ".terminfo" = {
      source = ../dotfiles/.terminfo;
      recursive = true;
    };

    bin = {
      source = ../bin;
    };
  };
}
