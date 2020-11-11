{ pkgs, ... }:
let
  keys = import ../keys.nix;
in {
  time.timeZone = "America/Toronto";

  boot = {
    cleanTmpDir = true;
    kernel.sysctl = {
      "fs.inotify.max_user_watches" = "1048576";
      "net.ipv4.ip_forward" = "1";
    };
  };

  security.sudo.wheelNeedsPassword = false;

  networking.firewall = {
    allowPing = true;
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (import ../overlays/mosh.nix)
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  environment.systemPackages = with pkgs; [
    bat
    curl
    direnv
    dive
    dnsutils
    docker
    docker-compose
    doctl
    file
    fish
    fzf
    gcc
    git
    gnumake
    gnupg
    go
    gopls
    htop
    inetutils
    ispell
    jq
    kubectl
    kubernetes-helm
    lua
    mariadb-client
    mosh
    nix-index
    nodejs
    openssl
    patchelf
    python
    python3
    ruby
    shellcheck
    starship
    syncthing
    tmux
    tree
    unzip
    wget
    z-lua
  ];

  programs = {
    fish.enable = true;
    mosh.enable = true;
  };

  services.emacs = {
    enable = true;
    package = pkgs.emacsGit-nox;
  };
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
  };

  virtualisation.docker.enable = true;

  users.mutableUsers = false;
  users.users.root = {
    openssh.authorizedKeys.keys = keys.sshPub;
  };
}
