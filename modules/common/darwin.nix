{ lib, pkgs, config, inputs, system, ... }:
with lib;
{
  imports = [ ./default.nix ];
  config = {
    nix.enable = false;
  };
}
