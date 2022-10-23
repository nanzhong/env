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
      fd
      gh
      git-filter-repo
      go
      gopls
      helix
      hunspell
      hunspellDicts.en-us
      hunspellDicts.en-us-large
      inotify-tools
      kubectl
      kubernetes-helm
      lazygit
      lua
      luajit
      mariadb-client
      neovim
      nodejs
      nodePackages.bash-language-server
      nodePackages.typescript-language-server
      nodePackages.vscode-langservers-extracted
      python
      python3
      ripgrep
      rubyPackages.solargraph
      ruby
      s3cmd
      shellcheck
      sqlite
      sqls
      starship
      sumneko-lua-language-server
      syncthing
      ttfautohint
      z-lua
    ];

    services.emacs = {
      enable = true;
      package = (pkgs.emacsGit-nox.override {
        nativeComp = true;
        withSQLite3 = true;
      } );
    };
    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
    };
  };
}
