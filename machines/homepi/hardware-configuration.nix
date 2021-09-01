{ modulesPath, ... }: {

  hardware = {
    raspberry-pi."4".fkms-3d.enable = true;
    enableRedistributableFirmware = true;
    pulseaudio.enable = true;
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  powerManagement.cpuFreqGovernor = "ondemand";
}
