{ self, inputs, pkgs, modulesPath, ... }: {
  imports = [
    inputs.raspberry-pi-nix.nixosModules.raspberry-pi
    inputs.raspberry-pi-nix.nixosModules.sd-image
    ./hardware-configuration.nix
    ./networking.nix
  ];

  nanzhong = {
    dev.enable = true;
    home =  {
      user = "nan";
      gitconfig = ".gitconfig.nan.devpi";
    };
  };

  system.stateVersion = "24.05";
}
