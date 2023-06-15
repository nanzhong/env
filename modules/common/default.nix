{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.nanzhong.common;
in {
  options = {
    nanzhong.common = {
      enable = mkEnableOption "Common configuration";
    };
  };

  config = mkIf cfg.enable {
    nix = {
      extraOptions = ''
        experimental-features = nix-command flakes auto-allocate-uids
        auto-allocate-uids = true
        bash-prompt-prefix = (nix:$name)\040
        extra-nix-path = nixpkgs=flake:nixpkgs
      '';

      package = pkgs.nixUnstable;

      gc = {
        automatic = true;
        interval = { Weekday = 0; Hour = 0; Minute = 0; };
        options = "--delete-older-than 30d";
      };
    };

    nixpkgs = {
      config.allowUnfree = true;
    };

    time.timeZone = "America/Toronto";

    environment.systemPackages = with pkgs; [
      _1password
      bat
      cachix
      curl
      dnsutils
      docker
      docker-compose
      file
      fish
      fishPlugins.forgit
      fishPlugins.fzf-fish
      fzf
      gcc
      git
      gnumake
      gnupg
      htop
      inetutils
      jq
      mosh
      nix-index
      openssl
      patchelf
      podman
      tmux
      tree
      unzip
      wget
    ];

    programs = {
      fish.enable = true;
    };
  };
}
