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
      inputs.emacs.overlay
    ];

    environment.systemPackages = with pkgs; [
      asciinema
      bat
      bazel-buildtools
      bazelisk
      btop
      carapace
      choose
      diffutils
      direnv
      nix-direnv
      fd
      jq
      gh
      gnumake
      go
      gopls
      hunspell
      hunspellDicts.en-ca-large
      hunspellDicts.en-us-large
      kubectl
      kubernetes-helm
      marksman
      markdown-oxide
      mariadb
      patchelf
      qemu
      ripgrep
      ruby
      s3cmd
      sbcl
      shellcheck
      sshuttle
      sqlite
      terraform
      unzip
    ];
  };
}
