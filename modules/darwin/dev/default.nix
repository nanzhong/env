{ lib, config, pkgs, ... }:
with lib;
let
  emacs-custom = (pkgs.emacs-git.override {
    withNS = true;
    withX = false;
    withGTK2 = false;
    withGTK3 = false;
  }).overrideAttrs (old: {
    patches = old.patches ++ [
#      ../../patches/emacs/frame-refocus.patch
    ];
  });
in
{
  imports = [ ../../common/dev/default.nix ];
  config = {
    environment.systemPackages = with pkgs; [
      emacs-custom
    ];

    # services.emacs = {
    #   enable = true;
    #   package = emacs-custom;
    # };
  };
}
