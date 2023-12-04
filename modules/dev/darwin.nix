{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.nanzhong.dev;
in
{
  imports = [ ./default.nix ];
  config = mkIf cfg.enable {
    environment = {
      pathsToLink = [ "/share/hunspell" ];
    };
  };
}
