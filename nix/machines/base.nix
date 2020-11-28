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
    dnsutils
    docker
    docker-compose
    file
    fish
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
    tmux
    tree
    unzip
    wget
  ];

  programs = {
    fish.enable = true;
    mosh.enable = true;
  };
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };

  virtualisation.docker.enable = true;

  users.mutableUsers = false;
  users.users.root = {
    openssh.authorizedKeys.keys = keys.sshPub;
  };
}
