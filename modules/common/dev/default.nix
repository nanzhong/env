{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.nanzhong.dev;
in {
  options.nanzhong.dev = {
    enable = mkEnableOption "Dev configuration";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      asciinema
      bat
      btop
      direnv
      fd
      jq
      gcc
      gnumake
      go
      gopls
      helix
      hunspell
      hunspellDicts.en-ca-large
      hunspellDicts.en-us-large
      kubectl
      kubernetes-helm
      patchelf
      ripgrep
      ruby
      s3cmd
      sbcl
      shellcheck
      sqlite
      unzip
    ];
  };
}
