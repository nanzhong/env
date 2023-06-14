{ self, pkgs, ... }: {
  nanzhong = {
    common.enable = true;
    dev.enable = true;
    home =  {
      user = "nan";
      group = "staff";
    };
  };

  nixpkgs = {
    overlays = [
      (import ../../overlays/alacritty.nix)
    ];
  };

  environment.shells = [ pkgs.fish ];
  environment.systemPackages = with pkgs; [
    alacritty
  ];

  services.tailscale.enable = true;
  services.nix-daemon.enable = true;
  system.checks.verifyBuildUsers = false;
}
