{ self, pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
  ];

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
