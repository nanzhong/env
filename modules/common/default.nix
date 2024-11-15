{ lib, config, pkgs, ... }:
with lib;
{
  config = {
    nix = {
      extraOptions = ''
        experimental-features = nix-command flakes
        bash-prompt-prefix = (nix:$name)\040
        extra-nix-path = nixpkgs=flake:nixpkgs
      '';

      package = pkgs.nixVersions.latest;

      gc = {
        automatic = true;
        options = "--delete-older-than 30d";
      };
    };

    nixpkgs = {
      config.allowUnfree = true;
    };

    time.timeZone = "America/Toronto";

    environment.systemPackages = with pkgs; [
      _1password
      cachix
      curl
      dnsutils
      file
      fish
      fishPlugins.forgit
      fishPlugins.fzf-fish
      fzf
      git
      gnupg
      inetutils
      mosh
      nix-index
      openssl
      podman
      tmux
      tree
      wget
      zoxide
    ];

    programs = {
      fish.enable = true;
      nix-ld.enable = true;
    };
  };
}
