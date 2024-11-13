{ ... }: {
  networking = {
    hostName = "devpi";
    networkmanager = {
      enable = true;
      ensureProfiles.profiles = {
        usb0-dhcp = {
          connection = {
            id = "usb0-dhcp";
            type = "ethernet";
            interface-name = "usb0";
            autoconnect-priority = 100;
            autoconnect-retries = 5;
          };
          ethernet = {};
          ipv4 = {
            dhcp-timeout = 3;
            method = "auto";
          };
          ipv6 = {
            addr-gen-mode = "default";
            method = "auto";
          };
          proxy = {};
        };
      };
    };
  };
}
