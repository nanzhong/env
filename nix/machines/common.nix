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

  environment.systemPackages = with pkgs; [
    curl
    dnsutils
    docker
    fish
    gcc
    git
    htop
    inetutils
    jq
    mosh
    openssl
    tmux
    tree
    wget
  ];

  programs = {
    fish.enable = true;
    mosh.enable = true;
  };

  services.openssh.enable = true;

  virtualisation.docker.enable = true;

  users.mutableUsers = false;

  users.users.root = {
    openssh.authorizedKeys.keys = keys.sshPub;
  };
}
