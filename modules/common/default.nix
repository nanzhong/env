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

      settings = {
        substituters = [
          "https://cache.nixos.org/"
          "https://nix-community.cachix.org"
        ];
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
         ];
      };
    };

    nixpkgs = {
      config.allowUnfree = true;
    };

    time.timeZone = "America/Toronto";

    environment = {
      systemPackages = with pkgs; [
        bash-env-json
        cachix
        curl
        dnsutils
        file
        fish
        fzf
        git
        gnupg
        inetutils
        mosh
        nix-index
        nushell
        openssl
        podman
        starship
        tmux
        tree
        wget
        zoxide
      ];

      shells = [ pkgs.nushell ];
    };
  };
}
