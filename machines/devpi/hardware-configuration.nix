{ ... }: {
  raspberry-pi-nix.board = "bcm2712";
  boot = {
    kernelModules = [ "libcomposite" ];
    kernelParams = [
      "modules-load=dwc2"
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
