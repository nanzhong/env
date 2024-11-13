{ ... }: {
  raspberry-pi-nix.board = "bcm2712";
  hardware = {
    bluetooth.enable = true;
  };
}
