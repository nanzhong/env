{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address="178.128.227.233"; prefixLength=20; }
          { address="10.20.0.5"; prefixLength=16; }
        ];
        ipv6.addresses = [
          { address="2604:a880:cad:d0::736:7001"; prefixLength=64; }
          { address="fe80::64b5:37ff:fe2d:942c"; prefixLength=64; }
        ];
        ipv4.routes = [ { address = "178.128.224.1"; prefixLength = 32; } ];
        ipv6.routes = [ { address = "2604:a880:cad:d0::1"; prefixLength = 32; } ];
      };
    };
    usePredictableInterfaceNames = lib.mkForce true;

    hostName = "dev";

    nameservers = [
      "67.207.67.3"
      "67.207.67.2"
    ];
    defaultGateway = "178.128.224.1";
    defaultGateway6 = "2604:a880:cad:d0::1";
    dhcpcd.enable = false;

    firewall = {
      allowedUDPPorts = [ 137 138 ];
      allowedTCPPorts = [ 139 445 3000 3001 8080 ];
    };
  };

  services.udev.extraRules = ''
    ATTR{address}=="66:b5:37:2d:94:2c", NAME="eth0"
    ATTR{address}=="f6:4a:89:f9:42:26", NAME="eth1"
  '';
}
