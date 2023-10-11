{ self, inputs, pkgs, modulesPath, ... }: {
  imports = [
    "${inputs.nixos-hardware}/raspberry-pi/4"
    ./hardware-configuration.nix
    ./networking.nix
  ];

  system.stateVersion = "22.05";

  boot = {
    kernelPackages = pkgs.linuxPackages_rpi4;
    initrd.availableKernelModules = [ "usbhid" "usb_storage" ];
    # ttyAMA0 is the serial console broken out to the GPIO
    kernelParams = [
        "8250.nr_uarts=1"
        "console=ttyAMA0,115200"
        "console=tty1"
        # Some gui programs need this
        "cma=128M"
    ];

    loader = {
      raspberryPi = {
        enable = true;
        version = 4;
      };
      grub.enable = false;
      generic-extlinux-compatible.enable = false;
    };

    tmp.useTmpfs = true;
  };

  nanzhong = {
    home =  {
      user = "nan";
    };
  };

  environment.systemPackages = with pkgs; [
    ffmpeg_6-full
    libraspberrypi
    rtorrent
    tailscale
  ];

  services.tailscale.enable = true;
}
