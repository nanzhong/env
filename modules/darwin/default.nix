{ lib, pkgs, config, inputs, system, ... }:
with lib;
let
  mkalias = inputs.mkAlias.outputs.apps.${system}.default.program;
in {
  imports = [ ../common/default.nix ];
  config = {
    nix.useDaemon = true;
    nix.gc.interval = { Weekday = 0; Hour = 0; Minute = 0; };

    # This fixes the inpcorrect path ordering
    programs.fish.loginShellInit = ''for p in (string split " " $NIX_PROFILES); fish_add_path --prepend --move $p/bin; end'';

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
