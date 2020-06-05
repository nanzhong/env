{ pkgs, ... }:
let
  keys = import ../keys.nix;
in {
  time.timeZone = "America/Toronto";

  boot.cleanTmpDir = true;
  
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
