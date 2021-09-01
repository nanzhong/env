{ lib, ... }: {
  networking = {
    hostName = "homepi";
    dhcpcd.enable = true;
    networkmanager = {
      enable = true;
    };
  };
}
