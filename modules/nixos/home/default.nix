{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.nanzhong.home;
in {
  imports = [ ../../common/home/default.nix ];
  config = {
    users.users = {
      "${cfg.user}" = {
        isNormalUser = true;
        home = "/home/${cfg.user}";
        description = "Nan Zhong";
        extraGroups = [ "wheel" "docker" ];
        shell = pkgs.fish;
        openssh.authorizedKeys.keys = cfg.keys;
      };

      root = {
        openssh.authorizedKeys.keys = cfg.keys;
      };
    };
  };
}
