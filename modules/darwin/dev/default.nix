{ lib, config, pkgs, ... }:
with lib;
let
  emacs-custom = (pkgs.emacs-git.override {
    withNS = true;
    withX = false;
    withGTK2 = false;
    withGTK3 = false;
  }).overrideAttrs (old: {
    postPatch = concatStringsSep "\n" [
      old.postPatch
      ''
        echo "Patching emacs icon..."
        cp "${../../../patches/emacs/Emacs.icns}" "nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns"
        cp "${../../../patches/emacs/emacs.tiff}" "nextstep/GNUstep/Emacs.base/Resources/emacs.tiff"
        cp "${../../../patches/emacs/emacs.svg}" "etc/images/icons/hicolor/scalable/apps/emacs.svg"
        cp "${../../../patches/emacs/emacs.ico}" "etc/images/icons/hicolor/scalable/apps/emacs.ico"
        cp "${../../../patches/emacs/emacs-128.png}" "etc/images/icons/hicolor/128x128/apps/emacs.png"
        cp "${../../../patches/emacs/emacs-48.png}" "etc/images/icons/hicolor/48x48/apps/emacs.png"
        cp "${../../../patches/emacs/emacs-32.png}" "etc/images/icons/hicolor/32x32/apps/emacs.png"
        cp "${../../../patches/emacs/emacs-24.png}" "etc/images/icons/hicolor/24x24/apps/emacs.png"
        cp "${../../../patches/emacs/emacs-16.png}" "etc/images/icons/hicolor/16x16/apps/emacs.png"
      ''
    ];
  });
in
{
  imports = [ ../../common/dev/default.nix ];
  config = {
    environment.systemPackages = with pkgs; [
      emacs-custom
    ];

    services.emacs = {
      enable = true;
      package = emacs-custom;
    };
  };
}
