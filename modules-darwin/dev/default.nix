{ lib, config, pkgs, ... }:
with lib;
let
  emacs-custom = pkgs.emacs-git.override {
    withNS = true;
    withX = false;
    withGTK2 = false;
    withGTK3 = false;
  };
in
{
  imports = [ ../../modules/dev/default.nix ];
  config = {
    environment.systemPackages = with pkgs; [
    # emacs-custom
    ];

    # services.emacs = {
    #   enable = true;
    #   package = emacs-custom;
    # };

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
          "with-modern-icon-elrumo2"
        ];
        restart_service = "changed";
      }
    ];
  };
}
