{ pkgs, ... }: {
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  home.packages = with pkgs; [
    breezy
    curl
    direnv
    dnsutils
    docker-compose
    doctl
    fly
    fzf
    git
    git-crypt
    gnumake
    gnupg
    go
    htop
    inetutils
    ispell
    jq
    kubectl
    kubernetes-helm
    lua
    mercurial
    nodejs
    openssl
    python
    rsync
    ruby
    starship
    tree
    unzip
    wget
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit-nox;
  };
  services.emacs.enable = true;

  home.file = {
    ".config" = {
      source = ./dotfiles/.config;
      recursive = true;
    };

    ".tmux.conf" = {
      source = ./dotfiles/.tmux.conf;
    };

    ".gitconfig" = {
      source = ./dotfiles/.gitconfig;
    };

    ".gitignore" = {
      source = ./dotfiles/.gitignore;
    };

    ".terminfo" = {
      source = ./dotfiles/.terminfo;
      recursive = true;
    };

    bin = {
      source = ./bin;
    };
  };
}
