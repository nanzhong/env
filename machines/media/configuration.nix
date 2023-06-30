{ self, pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
  ];

  system.stateVersion = "23.11";

  nanzhong = {
    home =  {
      user = "nan";
      group = "plex";
    };
  };

  environment.systemPackages = with pkgs; [
    plex
  ];

  services.tailscale.enable = true;
  services.plex = {
    enable = true;
    openFirewall = true;
  };
  services.openssh.extraConfig = ''
    Match Group plex
      ChrootDirectory /mnt/media
      ForceCommand internal-sftp
      AllowTcpForwarding no
  '';
}
