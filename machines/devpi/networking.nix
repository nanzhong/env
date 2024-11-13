{ ... }: {
  networking = {
    hostName = "devpi";
    dhcpcd.enable = true;
  };
}
