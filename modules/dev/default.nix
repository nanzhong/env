{ lib, config, pkgs, inputs, system, ... }:
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
      abseil-cpp
      asciinema
      bat
      bazel-buildtools
      bazelisk
      btop
      buf
      carapace
      choose
      cue
      diffutils
      direnv
      nix-direnv
      fd
      jq
      gh
      gnumake
      go
      gopls
      gum
      hunspell
      hunspellDicts.en-ca-large
      hunspellDicts.en-us-large
      jujutsu
      inputs.jjui.packages.${system}.default
      kind
      kubectl
      kubernetes-helm
      marksman
      markdown-oxide
      mariadb
      nodejs
      neovim
      opencode
      patchelf
      pnpm
      python3
      qemu
      ripgrep
      ruby
      s3cmd
      sbcl
      sshuttle
      shellcheck
      sqlite
      svelte-language-server
      terraform
      unzip
    ];
  };
}
