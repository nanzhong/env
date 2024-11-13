{ self, inputs, pkgs, modulesPath, ... }: {
  imports = [
    inputs.raspberry-pi-nix.nixosModules.raspberry-pi
    inputs.raspberry-pi-nix.nixosModules.sd-image
    self.nixosModules.sd-image
    ./hardware-configuration.nix
    ./networking.nix
  ];

  nanzhong = {
    dev.enable = true;
    home =  {
      user = "nan";
    };
  };

  system.stateVersion = "24.05";
}
