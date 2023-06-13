{ lib, config, pkgs, ... }:
with lib;
{
  imports = [ ../../modules/common/default.nix ];
  config = {
    homebrew.enable = true;
  };
}
