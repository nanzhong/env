{ lib, pkgs, config, inputs, system, ... }:
with lib;
{
  imports = [ ./default.nix ];
  config = {
    # This is a workaround to setup finder aliases for gui applications so that spotlight can find them
    system.activationScripts.postUserActivation.text = ''
      apps_source="${config.system.build.applications}/Applications"
      moniker="Nix Trampolines"
      app_target_base="$HOME/Applications"
      app_target="$app_target_base/$moniker"
      mkdir -p "$app_target"
      ${pkgs.rsync}/bin/rsync --archive --checksum --chmod=-w --copy-unsafe-links --delete "$apps_source/" "$app_target"
    '';
    nix.enable = false;
  };
}
