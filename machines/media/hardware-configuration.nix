{ modulesPath, ... }: {
  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];
  boot.loader.grub.device = "/dev/vda";
  fileSystems."/" = { device = "/dev/vda1"; fsType = "ext4"; };
  fileSystems."/mnt/media" = {
    device = "/dev/disk/by-id/scsi-0DO_Volume_media";
    fsType = "ext4";
    mountPoint = "/mnt/media";
    options = [ "defaults" "nofail" "discard" ];
  };
}
