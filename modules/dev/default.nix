{ lib, config, pkgs, inputs, ... }:
with lib;
let
  cfg = config.nanzhong.dev;
in {
  options.nanzhong.dev = {
    enable = mkEnableOption "Dev configuration";
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      inputs.helix.overlays.default
      (final: prev: {
        neovim = inputs.neovim.packages.${prev.system}.neovim;
        zig = inputs.zig.packages.${prev.system}.master;
        zls = inputs.zls.packages.${prev.system}.zls;
      })
    ];

    environment.systemPackages = with pkgs; [
      asciinema
      bat
      btop
      choose
      direnv
      fd
      jq
      gcc
      gh
      gnumake
      go
      gopls
      helix
      hunspell
      hunspellDicts.en-ca-large
      hunspellDicts.en-us-large
      kubectl
      kubernetes-helm
      neovim
      patchelf
      ripgrep
      ruby
      s3cmd
      sbcl
      shellcheck
      sshuttle
      sqlite
      unzip
      zig
      zls
    ];
  };
}
