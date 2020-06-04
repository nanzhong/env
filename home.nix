{ pkgs, ... }: {
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  home.packages = with pkgs; [
    curl wget rsync
    jq
    unzip
    tree
    ispell
    htop
    dnsutils inetutils
    git git-crypt breezy mercurial
    gnupg
    go
    python
    lua
    ruby
    nodejs
    docker-compose
    kubectl kubernetes-helm
    doctl
    fly
    fzf
    starship
    direnv
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
