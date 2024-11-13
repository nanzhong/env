{ ... }: {
  raspberry-pi-nix.board = "bcm2712";
  boot = {
    kernelParams = [
      "modules-load=dwc2,g_ether"
    ];
  };
  hardware = {
    bluetooth.enable = true;
    raspberry-pi.config = {
      all = {
        dt-overlays = {
          dwc2 = {
            enable = true;
            params = {};
          };
        };
      };
    };
  };
}
