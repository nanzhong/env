user: { pkgs, ... }: {
  home-manager.users."${user}" = {
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
