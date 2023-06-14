{ lib, config, pkgs, ... }:
with lib;
{
  imports = [ ../../modules/dev/default.nix ];
  config = {
    homebrew.taps = [
      "daviderestivo/emacs-head"
    ];

    homebrew.brews = [
      {
        name = "emacs-head@30";
        args = [
          "with-cocoa"
          "with-native-comp"
          "with-native-full-aot"
          "with-tree-sitter"
          "with-modern-icon-elrumo1"
        ];
        restart_service = "changed";
      }
    ];
  };
}
