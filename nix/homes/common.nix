user: { pkgs, ... }: {
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  home-manager.users."${user}" = {
    home.packages = with pkgs; [
      bat
      direnv
      docker-compose
      doctl
      fzf
      gnumake
      gnupg
      go
      gopls
      ispell
      kubectl
      kubernetes-helm
      lua
      mariadb-client
      nodejs
      python
      ruby
      starship
      unzip
      z-lua
    ];

    programs.emacs = {
      enable = true;
      package = pkgs.emacsGit-nox;
    };
    services.emacs.enable = true;

    home.file = {
      ".config" = {
        source = ../../dotfiles/.config;
        recursive = true;
      };

      ".tmux.conf" = {
        source = ../../dotfiles/.tmux.conf;
      };

      ".gitignore" = {
        source = ../../dotfiles/.gitignore;
      };

      ".terminfo" = {
        source = ../../dotfiles/.terminfo;
        recursive = true;
      };

      "bin/cpcat" = {
        source = ../../bin/cpcat;
      };
    };
  };
}
