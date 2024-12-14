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
