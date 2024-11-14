{ ... }:
let
  ip = "10.100.0.1";
in
{
  networking = {
    hostName = "devpi";
    hosts = {
      "127.0.0.1" = [ "devpi.lan" ];
      ip = [ "devpi.lan" ];
    };
    interfaces.usb0.ipv4.addresses = [
      {
        address = ip;
        prefixLength = 24;
      }
    ];
    dhcpcd.denyInterfaces = [ "usb0" ];
    wireless = {
      enable = true;
      userControlled.enable = true;
      secretsFile = "/home/nan/.config/wifi/secrets";
      networks = {
        "Happy Wifi, Happy Life".pskRaw = "ext:psk_home";
      };
    };
  };

  services.dnsmasq = {
    enable = true;
    settings = {
      domain = "devpi";
      domain-needed = true;
      dhcp-range = [ "10.100.0.100,10.100.0.200,255.255.255.0,24h" ];
      dhcp-option = [ "option:router,${ip}" ];
      interface = "usb0";
    };
  };

  systemd.services."usb-otg" = {
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    wantedBy = [ "default.target" ];
    script = ''
      mkdir -p /sys/kernel/config/usb_gadget/pi5
      cd /sys/kernel/config/usb_gadget/pi5
      echo 0x1d6b > idVendor # Linux Foundation
      echo 0x0104 > idProduct # Multifunction Composite Gadget
      echo 0x0103 > bcdDevice # v1.0.3
      echo 0x0320 > bcdUSB # USB2
      echo 2 > bDeviceClass

      mkdir -p strings/0x409
      echo "a0b1c2d3e4f5g6h7" > strings/0x409/serialnumber
      echo "RPI" > strings/0x409/manufacturer
      echo "PI5 USB Device" > strings/0x409/product

      mkdir -p configs/c.1/strings/0x409
      echo "CDC" > configs/c.1/strings/0x409/configuration
      echo 250 > configs/c.1/MaxPower
      echo 0x80 > configs/c.1/bmAttributes

      #ECM
      mkdir -p functions/ecm.usb0
      HOST="00:dc:c8:f7:75:15" # "HostPC"
      SELF="00:dd:dc:eb:6d:a1" # "BadUSB"
      echo $HOST > functions/ecm.usb0/host_addr
      echo $SELF > functions/ecm.usb0/dev_addr
      ln -s functions/ecm.usb0 configs/c.1/

      udevadm settle -t 5 || :
      ls /sys/class/udc > UDC
    '';
  };
  systemd.services.dhcpd4.after = [ "usb-otg.service" ];
  systemd.services."network-addresses-usb0".after = [ "usb-otg.service" ];
}
