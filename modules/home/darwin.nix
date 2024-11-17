{ lib, config, pkgs, inputs, ... }:
with lib;
let
  cfg = config.nanzhong.home;
in {
  imports = [
    inputs.home-manager.darwinModules.home-manager
    ./default.nix
  ];

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
