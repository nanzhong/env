{ lib, pkgs, config, inputs, system, ... }:
with lib;
let
  mkalias = inputs.mkAlias.outputs.apps.${system}.default.program;
in {
  imports = [ ./default.nix ];
  config = {
    nix.useDaemon = true;
    nix.gc.interval = { Weekday = 0; Hour = 0; Minute = 0; };

    # This fixes the inpcorrect path ordering from https://github.com/LnL7/nix-darwin/issues/122#issuecomment-1659465635
    programs.fish.loginShellInit =
      let
        # This naive quoting is good enough in this case. There shouldn't be any
        # double quotes in the input string, and it needs to be double quoted in case
        # it contains a space (which is unlikely!)
        dquote = str: "\"" + str + "\"";

        makeBinPathList = map (path: path + "/bin");
      in ''
      fish_add_path --move --prepend --path ${lib.concatMapStringsSep " " dquote (makeBinPathList config.environment.profiles)}
      set fish_user_paths $fish_user_paths
    '';

    # This is a workaround to setup finder aliases for gui applications so that spotlight can find them
    system.activationScripts.postUserActivation.text = ''
      apps_source="${config.system.build.applications}/Applications"
      moniker="Nix Trampolines"
      app_target_base="$HOME/Applications"
      app_target="$app_target_base/$moniker"
      mkdir -p "$app_target"
      ${pkgs.rsync}/bin/rsync --archive --checksum --chmod=-w --copy-unsafe-links --delete "$apps_source/" "$app_target"
    '';
  };
}
