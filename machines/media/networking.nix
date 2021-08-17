{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address="165.22.232.59"; prefixLength=20; }
          { address="10.20.0.7"; prefixLength=16; }
        ];
        ipv6.addresses = [
          { address="2604:a880:cad:d0::dab:1001"; prefixLength=64; }
          { address="fe80::8cff:d9ff:fe0e:71ae"; prefixLength=64; }
        ];
        ipv4.routes = [ { address = "165.22.224.1"; prefixLength = 32; } ];
        ipv6.routes = [ { address = "2604:a880:cad:d0::1"; prefixLength = 32; } ];
      };
    };
    usePredictableInterfaceNames = lib.mkForce false;

    hostName = "media";

    nameservers = [
      "67.207.67.2"
      "67.207.67.3"
    ];
    defaultGateway = "165.22.224.1";
    defaultGateway6 = "2604:a880:cad:d0::1";
    dhcpcd.enable = false;
  };

  services.udev.extraRules = ''
    ATTR{address}=="8e:ff:d9:0e:71:ae", NAME="eth0"
    ATTR{address}=="7e:67:c9:28:4c:90", NAME="eth1"
  '';
}
