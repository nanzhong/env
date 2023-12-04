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
    system.activationScripts.applications.text = mkForce ''
    echo "linking apps to /Applications..." >&2
    app_path="/Applications/Nix Apps"
    tmp_path="/tmp/nix-darwin-link-applications"

    rm -rf "$tmp_path"
    mkdir "$tmp_path"

    ${pkgs.fd}/bin/fd \
      -t l -d 1 . ${config.system.build.applications}/Applications \
      -x $DRY_RUN_CMD ${mkalias} -L {} "$tmp_path/{/}"

    $DRY_RUN_CMD rm -rf "$app_path"
    $DRY_RUN_CMD mv "$tmp_path" "$app_path"
  '';
  };
}
