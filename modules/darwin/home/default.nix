{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.nanzhong.home;
in {
  imports = [ ../../common/home/default.nix ];
  config = {
    users.users = {
      "${cfg.user}" = {
        home = "/Users/${cfg.user}";
        shell = pkgs.fish;
        openssh.authorizedKeys.keys = cfg.keys;
      };
    };
  };
}
